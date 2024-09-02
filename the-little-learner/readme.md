# Notes for _The Little Learner_

- https://www.thelittlelearner.com/
- https://github.com/themetaschemer/malt

## Setup

- [Racket](https://racket-lang.org/) 

- malt

  - install

    - raco pkg install malt

    - the [Git repository](https://github.com/themetaschemer/malt.git)

        - For MacOS and Linux: 

          ```
          git clone https://github.com/themetaschemer/malt.git
          cd malt
          make
          make install
          ```

        - For Windows:
          ```
          git clone https://github.com/themetaschemer/malt.git
          cd malt
          raco pkg install
          ```

  - Using in code
  
    ```scheme
    #lang racket
    (require malt)
    ```
  
    
  
  - Reference
  
    - [Documentation for Malt in the standard Racket format](https://docs.racket-lang.org/malt/index.html).



## Notation

See [notation.md](./notation.md)

## Laws | Rules

### Chapter 2

#### The Rule of Rank

A tensor's rank is the number of left square brackets before its leftmost scalar.

ch2:23

#### The Rule of Members and Elements

Non-empty lists have members and non-scalar tensors have elements.

ch2:37

#### The Rule of Uniform Shape

All elements of a tensor must have the same shape.

ch2:40

#### The Law of Rank and Shape

The rank of a tensor is equal to the length of its shape.

ch2:42

#### The Law of Simple Accumulator Passing

In a simple accumulator passing function definition every recursive function invocation is unwrapped, and the definition has at most one argument that **does not change**; an argument that **changes towards a true** base test; and another that **accumulates** a result.

ch2:46 Page 43

### The Law of Sum

For a tensor $ t $ with rank $ r > 0 $, the rank of $ (sum\ t) $ is $ r - 1 $.

ch3a:26 

### The Law of Revision

$$  \text{new } \theta_i = \theta_i - (\alpha \times \text{rate of change of loss w.r.t. } \theta_i)  $$

> w.r.t.: with respect to

$$  \theta_i \leftarrow \theta_i - \alpha \frac{\partial \text{Loss}}{\partial \theta_i}  $$

### The Rule of Hyperparameters

Every hyperparameter either is a scalar or has no value.

### Chapter 5

#### The Rule of Data Sets

In a data set `(xs, ys)`, both `xs` and `ys` must have the same number of elements. 

The elements of `xs`, however, can have a different shape from the elements of `ys`.

#### The Rule of Parameters (Final Version)

Every parameter is a tensor.

#### The Rule of *θ*

***θ*** is a list of parameters that can have different shapes.

### Chapter 6

#### The Rule of Batches

A batch of indices consists of random indices that are natural numbers smaller than `(tlen t)`

#### The Law of Batch Sizes

Each revision in stochastic gradient descent uses only a batch of size *batch-size* from the data set and the ranks of the tensors in the batch are the same as the ranks of the tensors in the data set.

ch6:38

### Chapter 7

#### The Law of Revisions

As long as we make sure that *gradient-descent* accepts an initial ***θ*** and results in a well-fitted ***θ***, any reasonable way of revising it from the first to the last revision is okay.

#### The Law of the Crazy “ates”

For any representation, the three “ates” are concerned with only one parameter and its accompaniments, and are not directly concerned with either ***θ*** or Θ.

### Chapter 9

#### The Law of Gradient Descent

The ***θ*** for a target function is learned by using one of the gradient descent functions.

### Chapter 10

#### The Rule of Artificial Neurons

An artificial neuron is a parameterized linear function composed with a nonlinear decider function

## Chapter Guide

https://www.thelittlelearner.com/#ChapterGuide

## Errata

https://www.thelittlelearner.com/#Errata

## Index

> The number in the first-level list represents the chapter. And `<number> a` is an Interlude before Chapter ` <number>`. For example, `3a` indicates an Interlude before Chapter 3.
>
> In the second-level of list, the number following each term represents the frame number within the corresponding chapter


- 1

  - `(line-xs, line-ys)`
  - `line`
  - $l_i$
  - `(list m ...)`

- 2
  - *scalar?* 9
  - [*e es* …] (where *e es* … are its elements) 10
  - ![tlent.png](assets/tlent.png) 17
  - $ t|_i $ 24
  - *shape* 37
  - *scons* 40
  - $|ls|$ 42
  - rank 44 [^rank]
- 3a

  - `sum` 24

- 3 Running Down a Slippery Slope | Lossy Toys

  - `l2-loss` 22

- 4 Slippery Toys
  - ∇ 16
  - revise 24
  - map 25
  - gradient descent(draft) 46
- 5a Hyperactive Toys
  - **declare-hyper** 8
  - **with-hypers** 9
- 5 Toys for Target Practice
  - gradient-descent (with hyperparameters `revs` and $alpha$) 5
  - `(quad-xs, quad-ys)` 8
  - `quad` 15
  - `(plane-xs, plane-ys)` 23
  - `plane` 25
  - • (`dot-product`) 26
- 6a Interlude III: The Shape of Things to Come
  - matrix 4
  - column matrix 6
  - row matrix 7
- 6 Random Toys (An Apple a Day)
  - samples 24
  - $ t||_b$ 27
  - batch-size 34
  - sampling-obj 36
- 7 Crazy Toys
  - gradient-descent 38
  - naked-gradient-descent 43
- 8 Fast Toys
  - $ \mu $
  - *velocity-gradient-descent*
- 9a Smooth Toys
  - smooth
- 9 Be Adamant | Faster Toys
  - $ \beta $
  - `rms-gradient-descent`
  - `adam-gradient-descent`
- [10a More Extendy Toys](10aExtensioMagnifico\readme.md)
  - $ext1$ 23
    - extending f with one argument
  - $ sqrt^0 $ 23
    - `of-rank`
  - $zeroes^0$ 17(draft) 23
  - $sum^1$ 24
  - $ flatten^2 $ 27
  - $ ext2 $ 35
  - $+^{0,0}$ 
  - $*^{0,0}$
  - $sqr$
  - $*^{2,1}$
- 10 Neural Toys
  - rectify
  - $linear^{1, 1}$
  - $relu^{1,1}$
- 11 Shapey Toys
  - $\bullet^{2,1} \quad 219$
  - $linear$
  - $relu$
  - $l_{i\downarrow} \quad (\text{where } l \text{ is nonempty and } i \text{ is positive}) \quad $ 
  - $$  \textit{k-relu} \quad $$
- 12 Blocky Toys

  - block
  - block-fn
  - block-ls
  - stack-blocks
  - dense-block
- 13 Classy Toys

  - init-θ 262
  - zero-tensor 263
  - random-tensor 264
  - (iris-test-xs, iris-test-ys) 265
  - (iris-train-xs, iris-train-ys) 265
  - iris-classifier 265
  - iris-θ-shapes 265
  - iris-θ 266
  - model 267
  - iris-model 267
- 14a Training Toys

  - accuracy
  - grid-search
- 14b Zippy Toys

  - zipping signals
- 14 Slidy Toys

  - (morse-xs morse-ys)
  - correlate
- 15 Correlated Toys

  - corr
  - recu
  - skip
  - skip-block
- 



[^rank]: order of tensor, not Rank in matrix





