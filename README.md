# haskey

haskey is a programming language written in Haskell.

It is a language that originally appeared in the book "Writing An Interpreter In Go".

It is originally written in Go.

## rule
Go言語でつくるインタプリタ本と同じ条件とするため以下のルールとします

- base パッケージのみ使用する
    - text、bytestring などは例外でOK
- Parsec は使わない
    - パーサコンビネータは使わない
    - もちろん Alex、Happy などの字句解析、構文解析系のライブラリも使用しない

## Copyright
原作：動物のお医者さん
アスキーアート：aahub
