# Public API

Status: frozen at 0.1.0.

fortad transforms source text. It returns generated Fortran and has no runtime
tape. A breaking change to the symbols or command-line options below requires a
version change and a roadmap entry.

## Result and version

```fortran
type :: fad_result_t
    logical :: ok = .false.
    character(len=:), allocatable :: code
    character(len=:), allocatable :: message
end type fad_result_t

function fad_version() result(version)
```

On success, `ok` is true and `code` contains generated source. On failure,
`ok` is false and `message` names the refused construct. `fad_version()`
returns `"0.1.0"` in this release.

## Transformation functions

All character arguments accept ordinary Fortran strings. Omit an optional
argument to select its default. The 0.1.0 entry points do not normalize blank
optional values consistently, so a blank string is not a portable substitute
for omission.

```fortran
function fad_jvp(source, independents, name, suffix, n_directions, &
                 module_name, with_primal, from) result(res)
    character(len=*), intent(in) :: source
    character(len=*), intent(in) :: independents(:)
    character(len=*), intent(in), optional :: name, suffix, n_directions
    character(len=*), intent(in), optional :: module_name, from
    logical, intent(in), optional :: with_primal
    type(fad_result_t) :: res
end function
```

`fad_jvp` emits one forward product. Defaults are `<primal>_jvp` for `name`,
`_d` for `suffix`, and true for `with_primal`. Supplying `n_directions` enables
vector mode. Its value is the name of the generated integer dummy, such as
`"n_dir"`. Vector tangent arrays put this direction index first.

```fortran
function fad_vjp(source, independents, dependent, name, suffix, &
                 module_name, with_primal, from) result(res)
    character(len=*), intent(in) :: source
    character(len=*), intent(in) :: independents(:)
    character(len=*), intent(in), optional :: dependent, name, suffix
    character(len=*), intent(in), optional :: module_name, from
    logical, intent(in), optional :: with_primal
    type(fad_result_t) :: res
end function
```

`fad_vjp` emits one reverse product. Defaults are `<primal>_vjp` for `name`,
`_b` for `suffix`, and true for `with_primal`. `dependent` defaults to a
function result or the sole `intent(out)` dummy. Supply it when the primal has
several possible outputs.

```fortran
function fad_hvp(source, independents, dependent, name, module_name, &
                 from) result(res)
    character(len=*), intent(in) :: source
    character(len=*), intent(in) :: independents(:)
    character(len=*), intent(in), optional :: dependent, name
    character(len=*), intent(in), optional :: module_name, from
    type(fad_result_t) :: res
end function
```

`fad_hvp` composes forward mode over generated reverse source. The current
default generated name is `fad_hvp`. The function accepts one direction and
has no vector-direction or derivative-only option.

```fortran
function fad_taylor(source, independents, order_name, name, module_name, &
                    from) result(res)
    character(len=*), intent(in) :: source
    character(len=*), intent(in) :: independents(:)
    character(len=*), intent(in), optional :: order_name, name
    character(len=*), intent(in), optional :: module_name, from
    type(fad_result_t) :: res
end function
```

`fad_taylor` transforms straight-line scalar source. `order_name` defaults to
`order`, and `name` defaults to `<primal>_taylor`. Generated code calls the
public `tay_*` arithmetic routines.

```fortran
function fad_roundtrip(source, from) result(res)
    character(len=*), intent(in) :: source
    character(len=*), intent(in), optional :: from
    type(fad_result_t) :: res
end function
```

`fad_roundtrip` parses and emits the selected primal without differentiation.
It is the source-pipeline diagnostic used before testing derivatives.

For every transformation, `from` selects a procedure in multi-procedure input.
The first procedure is the default. Other procedures in the source remain
available for bounded same-file inlining. `module_name` wraps the generated
procedure in a module and gives consumers a compiler-checked interface. If
`module_name` and the generated procedure name differ only by letter case, the
wrapper is named `<module_name>_module`. The procedure name is preserved.

The generated argument list follows the primal arguments and the derivative
objects required by the selected mode. Inspect `res%code` before writing a
caller. The [Rosenbrock example](../../example/README.md) shows a complete VJP
call.

## Rule registry

```fortran
subroutine fad_add_rule(name, partials, stat)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: partials(:)
    integer, intent(out), optional :: stat
end subroutine

subroutine fad_add_call_rule(name, n_args, tangent, adjoint, stat)
    character(len=*), intent(in) :: name
    integer, intent(in) :: n_args
    character(len=*), intent(in) :: tangent(:), adjoint(:)
    integer, intent(out), optional :: stat
end subroutine

subroutine fad_clear_rules()
subroutine fad_register_blas_lapack_rules(stat)
    integer, intent(out), optional :: stat
end subroutine
```

`fad_add_rule` registers one scalar partial expression for each argument.
Expressions use `$1`, `$2`, and later argument placeholders:

```fortran
call fad_add_rule("eos_pressure", &
                  ["deos_drho($1, $2)", "deos_dtemp($1, $2)"], stat)
```

`fad_add_call_rule` registers statement templates. `$k` names the k-th actual
argument, `$kd` its tangent, and `$kb` its adjoint. fortad performs textual
substitution. It does not parse the templates or validate their derivatives.

`fad_clear_rules` removes scalar and statement rules. A later registration of
the same name replaces the earlier registration. The registry is process
state, so clear and populate it before a transformation instead of modifying it
concurrently.

`fad_register_blas_lapack_rules` installs the opt-in `dgesv` statement rule.
Call it after `fad_clear_rules`. The generated code needs explicit BLAS/LAPACK
interfaces and linked libraries. See the
[`dgesv` contract](blas-lapack-rules.md).

## Static patterns and sparse recovery

```fortran
subroutine fad_static_pattern(source, independents, dependents, pattern, &
                              stat, message, from)
```

`fad_static_pattern` returns a conservative structural Jacobian pattern for a
supported lowered procedure. `from` selects the procedure. `stat` is zero on
success, and the optional allocatable `message` describes a failure.

```fortran
type :: sparsity_t
    integer :: n_rows = 0
    integer :: n_cols = 0
    integer, allocatable :: col_start(:)
    integer, allocatable :: rows(:)
end type sparsity_t

subroutine colour_columns(pattern, colour, n_colours, stat)
subroutine seed_matrix(pattern, colour, n_colours, seeds)
subroutine recover_entries(pattern, colour, compressed, values)
subroutine star_colour_columns(pattern, colour, n_colours, stat)
subroutine recover_symmetric(pattern, colour, compressed, values, stat)
```

`col_start` and `rows` store the nonzero row indices for each column.
`seed_matrix` returns `seeds(n_colours, n_cols)`. Compressed results use
`compressed(n_colours, n_rows)`. Star coloring and symmetric recovery require a
symmetric pattern.

The [sparse product guide](../products.md#sparse-derivatives) gives the call
order and the scalar-HVP loop required for Hessians.

## Checkpoint schedules

```fortran
type :: revolve_action_t
    integer :: kind = 0
    integer :: from = 0
    integer :: to = 0
    integer :: slot = 0
end type revolve_action_t

type :: revolve_t
    integer :: n_steps = 0
    integer :: n_slots = 0
    integer :: n_actions = 0
    integer :: forward_steps = 0
    type(revolve_action_t), allocatable :: actions(:)
end type revolve_t

subroutine revolve_schedule(n_steps, n_slots, schedule, stat)
```

The action constants are `REV_ADVANCE`, `REV_TAKESHOT`, `REV_RESTORE`, and
`REV_TURN`. The caller executes the returned actions and owns checkpoint
storage.

## Taylor arithmetic

The fixed-order coefficient routines exported through `fortad` are:

```fortran
tay_const(value, z)
tay_var(value, direction, z)
tay_add(a, b, z)
tay_sub(a, b, z)
tay_scale(c, a, z)
tay_mul(a, b, z)
tay_div(a, b, z)
tay_exp(a, z)
tay_log(a, z)
tay_sqrt(a, z)
tay_sin_cos(a, s, c)
tay_pow_int(a, p, z)
value = tay_derivative(z, k)
```

Coefficient arrays have lower bound zero. `tay_var` seeds the value and first
directional coefficient. `tay_derivative` multiplies coefficient `k` by `k!`.

## CLI

```text
fortad FILE [NAMES] [OPTIONS]
fortad jvp|vjp|hvp FILE [NAMES] [OPTIONS]
fortad jvp|vjp|hvp NAMES [OPTIONS] FILE
fortad all FILE [NAMES]
fortad check [--proc NAME] [--output PATH] FILE
fortad --indep NAMES [OPTIONS] FILE
fortad [PRODUCT] --head 'NAME(arg1 arg2)' [OPTIONS] FILE
```

The shortest source-first form is `fortad kernel.f90`, which emits a JVP. The
source-first form also accepts `--mode reverse` or `--mode hessian`, so
`fortad kernel.f90 --mode reverse` writes the inferred VJP. The explicit
product form `fortad vjp kernel.f90` selects reverse mode. In both
forms, when `NAMES` is omitted, FortAD lowers the first procedure (or the one
named by `--proc`), infers its non-`intent(out)` dummies, uses the function
result or sole `intent(out)` dummy as the reverse dependent, and chooses these
defaults:

```text
generated procedure: <procedure>_<product>
wrapper module:      <source-stem>_<product>_mod
output file:         <source-stem>_<product>.f90
```

The source stem is sanitized into a Fortran identifier for the module. Use
`--verbose` to print the decisions. Explicit names, `--proc`, `--dep`,
`--name`, `--module`, and `--output` override the inferred values. If the
source does not provide enough information, the command fails and the
explicit forms remain available. The names-first compact form,
`fortad vjp x --dep y kernel.f90`, and the original flag form remain stable for
existing scripts.

Tapenade-style head specifications are accepted as a shorter migration path:
`--head 'NAME(arg1 arg2)'` (or `-head`) is shorthand for selecting `NAME` with
the listed active arguments. Comma separators are accepted as well. A head
without parentheses, such as `--head NAME`, selects the procedure and leaves
independent-variable inference enabled. The inferred procedure name, wrapper
module, and sibling output path are filled in exactly as for source-first
syntax; later explicit `--proc`, `--name`, `--module`, or `--output` options
still override the corresponding defaults.

Source-first parsing is selected when the first positional argument is an
existing file, including the bare `fortad FILE` form. Supplying two positional
paths is rejected as ambiguous instead of choosing one silently. The legacy
flag form still accepts its input path after the options.

Values must be separate arguments. `--mode reverse` is accepted, while
`--mode=reverse` is not. In the names-first compact form, the independent-name
list follows the product immediately; in the source-first form, the existing
input path follows the product and the names follow it. The bare source-first
form accepts `--mode` and `--indep` after the path; the explicit `jvp`, `vjp`,
and `hvp` forms reject those redundant selectors instead of silently choosing
one spelling. `--roundtrip` remains standalone. The product names are
deliberately distinct from the legacy mode values, so an existing positional
input path named `forward`, `reverse`, or `hessian` keeps its old meaning.

`fortad check FILE` is the compact spelling of the existing `--roundtrip`
operation. It parses the file and re-emits its default procedure, or the one
selected by `--proc`, and accepts `--output`. Derivative-only options are
rejected. Success means the FortAD parser/normalizer round-trip completed for
that procedure. It is not a claim that a Fortran compiler accepts the whole
file or that FortAD can differentiate it.

`fortad all FILE` runs the inferred JVP and VJP transformations together and
writes `<source-stem>_jvp.f90` and `<source-stem>_vjp.f90`, with distinct
`<source-stem>_jvp_mod` and `<source-stem>_vjp_mod` wrappers. It accepts
positional independent names plus `--proc`, `--dep`, `--directions`,
`--no-primal`, and `--verbose`. `--output`, `--name`, and `--module` are
rejected because one path or name would be ambiguous for two products.

| Option | Scope | Meaning |
| --- | --- | --- |
| `jvp`, `vjp`, `hvp` | compact derivative form | product followed by `FILE [NAMES]` or `NAMES ... FILE` |
| bare `FILE` | inferred JVP form | infer the first procedure and write `<stem>_jvp.f90` |
| `all` | compact paired-product form | infer and write both JVP and VJP siblings |
| `check` | compact round-trip form | validate and re-emit source without differentiation |
| `-i`, `--indep a,b` | legacy and bare source-first forms | explicit independent variables; source-first form can infer them |
| `-m`, `--mode MODE` | legacy and bare source-first forms | `forward`, `reverse`, or `hessian` |
| `--dep name` | reverse | dependent when the default is ambiguous |
| `-d`, `--directions name` | forward | generated direction-count dummy and vector mode |
| `--name name` | derivative modes | generated procedure name |
| `--module name` | derivative modes | generated wrapper module |
| `--proc name` | all transformations | target in multi-procedure input |
| `--head spec` / `-head spec` | all transformations | Tapenade-style `NAME(arg1 arg2)` procedure and active-argument shorthand |
| `--no-primal` | forward and reverse | omit results needed only for the primal value |
| `--verbose` | inferred source-first form | print selected procedure, names, module, and output path |
| `--roundtrip` | standalone mode | parse and emit without requiring `--indep` |
| `--rule spec` | current process | scalar rule in `NAME:partial;partial` form |
| `--call-rule spec` | current process | statement rule in `NAME:n_args:tangent;...|adjoint;...` form |
| `-o`, `--output path` | all transformations | output file instead of standard output |
| `--version` | standalone | print the version and exit |
| `-h`, `--help` | standalone | print built-in help and exit |

Shell-quote rule specifications because `$`, semicolons, and `|` have shell
meanings. The CLI accepts at most one input path. If several paths are supplied,
the last one wins.

## Product-name mapping

ADOL-C's driver names provide a useful product taxonomy. They are not literal
compatibility aliases:

| Product name | fortad surface |
| --- | --- |
| `jac_vec` | `fad_jvp` |
| `vec_jac` | `fad_vjp` |
| `hess_vec` | `fad_hvp` |
| `jacobian` | repeated or vector `fad_jvp`, with sparse recovery when applicable |
| `hessian` | repeated `fad_hvp`, with symmetric sparse recovery when applicable |
