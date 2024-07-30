# Notations

## Some chars

- ∇: nabla, used in vector calculus and partial differential equations.
- λ: work well in `.rkt` files as `lambda` keyword

## Transcribing to Scheme

We write some of our functions using a more compact notation so that they are easier to read and to fit snugly in the little boxes. Before running a program, be sure to transcribe our notation into Scheme code. The table below shows how to write these directly in Scheme.

The first column in the table below refers to the earliest occurrence in the book of the notation shown in the second column. The third column shows how to transcribe our programs, e.g., `[ 5 (+ 10 2) 28 ]` is transcribed as `(tensor 5 (+ 10 2) 28)`.

| Page:Frame | Notation                         | Transcription              |
|------------|----------------------------------|----------------------------|
| 24:17      | `[ t ts …]`                      | `(tensor t ts …)`          |
| 26:24      | $ l_i $                          | `(ref l i)`                |
| 27:27      | `(list m …)`                     | `(list m …)`               |
| 33:17      | ![tlent.png](assets/tlent.png) | `(tlen t)`                 |
| 36:24      | $ t                              |_i $                 | `(tref t i)`               |
| 41:42      | $                                |l|$            | `(len l)`                  |
| 52:22      | `⟨op⟩⟨rank⟩ t`                   | `(⟨op⟩-⟨rank⟩ t)`          |
| 77:15      | `(∇ f θ)`                        | `(gradient-of f θ)`        |
| 106:25     | `(• t u)`                        | `(dot-product t u)`        |
| 106:26     | `⟨op⟩⟨rank1⟩,⟨rank2⟩ t`          | `(⟨op⟩-⟨rank1⟩-⟨rank2⟩ t)` |
| 124:27     | $ t\|_i $                        | `(trefs t b)`              |
| 226:49     | `li↓`                            | `(refr l i)`               |

For example, on page 52, frame 22, we introduce a hyphen between `sum` and `1` to transcribe $ sum^1 $ to `sum-1`, and on page 106, frame 26, we introduce a second hyphen to transcribe `•1,1` to `dot-product-1-1`.

## Greek Letters and Notational Variants

Greek letters and notational variants of variable names are transcribed as follows:

- `â` → `a-hat`
- `α` → `alpha`
- `α` (notational variant) → `alpha-hat`
- `αν-α` → `an-alpha`
- `β` → `beta`
- `ĉ` → `c-hat`
- `ϵ` → `epsilon`
- `θ` → `theta`
- `Θ` → `big-theta`
- `λ` → `lambda`
- `μ` → `mu`
- `π` → `pi`

Unicode can be used in names of formals, functions, and keywords.

## Running the Code

The successful running of the code requires the installation of the Malt package for Racket v8.0 or later. Details on downloading and running the code can be found on [The Little Learner](http://www.thelittlelearner.com).