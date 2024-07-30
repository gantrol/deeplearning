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

## Chapter Guide

https://www.thelittlelearner.com/#ChapterGuide

## Erratta

https://www.thelittlelearner.com/#Errata