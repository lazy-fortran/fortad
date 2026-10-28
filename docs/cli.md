# Command-line interface

The command reads one Fortran source file. Explicit forms write generated
Fortran to standard output; the source-only compact form writes a predictable
derivative file beside the input. Start with one of these forms:

```console
$ fortad kernel.f90                       # infer inputs; write kernel_jvp.f90
$ fortad kernel.f90 --mode reverse         # same inference; write kernel_vjp.f90
$ fortad jvp kernel.f90                    # infer inputs; write kernel_jvp.f90
$ fortad vjp kernel.f90                    # infer inputs and the sole output
$ fortad hvp kernel.f90                    # infer inputs; write kernel_hvp.f90
$ fortad all kernel.f90                    # write both JVP and VJP siblings
$ fortad check kernel.f90                 # parser/round-trip check
```

## Inference of procedure and arguments

The shortest form, `fortad FILE`, infers the first procedure and its
differentiable dummy arguments and writes `<stem>_jvp.f90` beside the input.
Add a comma-separated positional name list only when you want to override the
inferred inputs. Use `fortad all FILE` when both JVP and VJP siblings are
needed; explicit product names and flags remain available for multi-procedure
files, custom interfaces, and registered rules.

The bare source-first form also accepts `--mode forward|reverse|hessian` and
`--indep NAMES`, so a reverse product with inferred output selection can be
written as `fortad kernel.f90 --mode reverse`. The explicit `jvp`, `vjp`, and
`hvp` forms remain strict compact spellings: their product name already fixes
the mode.

In a multi-procedure source, an explicit derivative output name can select the
procedure without a separate `--proc` flag. For example,
`fortad vjp source.f90 --output selected_vjp.f90` tries `selected` as the
procedure name, infers its inputs and output, and falls back to the first
procedure if that basename is not present. An explicit `--proc` always wins;
the output filename is only a conservative hint, never a guessed procedure.
This inference applies when the independent-name list is omitted; supplying
names explicitly keeps the existing selected-procedure behavior.

Reverse-mode inference also handles a legacy subroutine with no `INTENT`
annotations when it has one unambiguous output: `fortad vjp legacy.f90`
infers the written dummy as the dependent and excludes it from the
independents. Use `--dep NAME` when the procedure has multiple possible
outputs.

When reverse inference is ambiguous, FortAD refuses to guess and reports the
candidate output names in the diagnostic, followed by the exact `--dep NAME`
override to use. No generated file is written in that case.

## Derived-type arguments

When no `--indep` list is supplied, the CLI chooses concrete `REAL` component
paths actually read from derived-type dummies, such as `state%inner%q` or
`grid%x`. It never perturbs a whole derived object implicitly; whole-object
activity remains an explicit refusal because it can change dynamic type and
storage identity.

For legacy `-b`/`--reverse` subroutines without `INTENT`, a unique direct write
to a concrete, non-aliased `REAL` component such as `soldat(1)%a` is inferred as
the dependent. The generated VJP receives a separate shaped cotangent seed
for that component and one derived shadow for independent component adjoints;
it does not duplicate a shadow dummy or activate the whole object. Ambiguous,
aliased, polymorphic, allocatable, pointer, global, or multiply-written
component dependents remain named refusals.

## Tapenade-compatible options

For Tapenade-style scripts, `--head` (also `-head`) combines the procedure
and active arguments: `fortad vjp -head 'kernel(x)' source.f90`. Commas are
accepted too. The procedure name is equivalent to `--proc`; the names inside
the parentheses are equivalent to `--indep`, while omitted names still use
FortAD's inference.

The common Tapenade command shapes are also accepted directly. `-O` names
the output directory; `-o` is the Tapenade output stem, so FortAD adds
`_p`, `_d`, or `_b` and `.f90`.

| Tapenade command | FortAD command |
| --- | --- |
| `fortad -p -O out -o kernel source.f` | accepted directly; writes `out/kernel_p.f90` |
| `fortad -d -root kernel -O out -o kernel source.f` | accepted directly; writes `out/kernel_d.f90` |
| `fortad -b -root kernel -O out -o kernel source.f` | accepted directly; writes `out/kernel_b.f90` |
| `fortad -d -root kernel -multi -O out -o kernel source.f` | accepted directly; forward vector directions use `nd` |
| `-head 'kernel(x)'` | `--head 'kernel(x)'` or `-head 'kernel(x)'` |

Tapenade's current multidirectional spelling, `-vector`, is accepted as an
alias for FortAD's older `-multi` spelling. It automatically selects forward
vector mode with the `nd` direction-count dummy, so a normal invocation can
omit both `-root` (when `-o STEM` names the procedure) and `--directions`:
`fortad -d -vector -o kernel source.f`.

For a multi-procedure legacy file, `-root` can be omitted when `-o STEM`
matches the desired procedure name: `fortad -b -O out -o kernel source.f` selects
`kernel` automatically. If the stem does not match a procedure, the first
procedure remains the default.

For the usual single-procedure file, `fortad source.f90` is shorter still: it
infers the active dummies, procedure name, wrapper module, and output path.
Tapenade-style legacy subroutines without `INTENT` are handled by the direct
`-d`/`-b` aliases when FortAD can identify one dummy written before it is read;
ambiguous procedures still need `--head` or `--dep`.

`-ext FILE` is accepted as a migration aid, but it does not import Tapenade's
external intrinsic-summary format. Register the needed operation with
`--rule` or `--call-rule` instead. Tapenade's multidirectional reverse mode is
not silently emulated: `-vector` and `-multi` currently have forward-mode
semantics only.

The common `-context`, `-fixinterface`, and `-standalonediff` switches are
also accepted. They are no-ops because FortAD always lowers with the complete
source context, emits checked module interfaces, and writes standalone
Fortran source. This lets an existing Tapenade `Options` file remain usable
without a flag-filtering wrapper.

## Synopsis and overrides

With source-first compact syntax, omitted names are inferred from the selected
procedure's dummy arguments: explicit `intent(out)` dummies are outputs and
the other dummies are treated as differentiable inputs. The first procedure is
the default; `--proc NAME` selects another one. FortAD also chooses the
`<procedure>_<product>` symbol, a checked wrapper module, and a sibling output
file. Add `--verbose` to print those decisions. `--output PATH`, `--module
NAME`, `--name NAME`, `--dep NAME`, and explicit comma-separated names override
the defaults. The full option and custom-rule reference is in [the CLI API
section](design/public-api.md#cli). The flag form and names-first compact
form remain available for scripts and advanced rules:

```text
fortad FILE [NAMES] [OPTIONS]
fortad jvp|vjp|hvp FILE [NAMES] [OPTIONS]
fortad jvp|vjp|hvp NAMES [OPTIONS] FILE
fortad all FILE [NAMES]
fortad check [--proc NAME] [--output PATH] FILE
fortad --indep NAMES [OPTIONS] FILE
fortad [PRODUCT] --head 'NAME(arg1 arg2)' [OPTIONS] FILE
```

`fortad all FILE` is the short path when a workflow needs both forward and
reverse products. It infers the same procedure and active dummies, writes
`<stem>_jvp.f90` and `<stem>_vjp.f90` beside the input, and gives each a
distinct checked wrapper module. Use `jvp` or `vjp` separately when custom
names, modules, or output paths are needed.

`check` parses and re-emits the selected procedure without differentiating it.
A successful check establishes round-trip support for that procedure. It does
not establish whole-file compiler conformance or derivative support.

