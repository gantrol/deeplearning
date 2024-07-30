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

### The Law of Revision

$$  \text{new } \theta_i = \theta_i - (\alpha \times \text{rate of change of loss w.r.t. } \theta_i)  $$

> w.r.t.: with respect to

$$  \theta_i \leftarrow \theta_i - \alpha \frac{\partial \text{Loss}}{\partial \theta_i}  $$

### The Rule of Hyperparameters

Every hyperparameter either is a scalar or has no value.

## Chapter Guide

https://www.thelittlelearner.com/#ChapterGuide

## Erratta

https://www.thelittlelearner.com/#Errata

## Index


- 1
- 2
- 3a
- 3
- 4 Slippery Toys
  - ∇ 78
  - revise 80
  - map 81
  - gradient descent(draft) 89
- 4a Hyperactive Toys

  - **declare-hyper** 94
  - **with-hypers** 94