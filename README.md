# Haskey

haskey is a programming language written in Haskell.

It is a language that originally appeared in the book "Writing An Interpreter In Go".

It is originally written in Go.

## Rule
Go言語でつくるインタプリタ本と同じ条件とするため以下のルールとします

- base パッケージのみ使用する
    - text、bytestring などは例外でOK
- Parsec は使わない
    - パーサコンビネータは使わない
    - もちろん Alex、Happy などの字句解析、構文解析系のライブラリも使用しない

## Usage
```
Usage: haskey [FILE]
  Haskey is a programming language written in Haskell. If you don't any
  commands, repl runs.

Available options:
  -h,--help                Show this help text
  FILE                     input files
```

## Status
実装状況

- 値(式)
     - 変数
     - 関数
     - 数値リテラル
     - 文字列リテラル
     - 真偽値
     - 式の評価結果
- 代入
     - 式に属するものは全て代入可能
- if 式
     - ブロック内の評価結果を返す
     - 返すべき値がない場合は null を返す
- 演算
     - 四則演算
     - 論理演算
     - 文字列結合(+)
- 関数
     - 第一級オブジェクト
     - 高階関数サポート
     - クロージャサポート


## Examples
```
prompt$ haskey
Hello! This is the Haskey programming language!
Feel free to type in commands
Usage: haskey --help
>> 
let a = 100;
>> 
let b = 200;
>> 
a + b;
300
>> 
if(a > 1000){ true; }else{ false; };
False
>> 
let add = fn(x, y) { x + y };
>> 
let sub = fn(x, y) { x - y };
>> 
let applyFunc = fn(a, b, func) { func(a, b) };
>> 
applyFunc(2, 2, add);
4
>> 
applyFunc(10, 2, sub);
8
>> 
"Hello" + " " + "World";
Hello World
```


## Copyright
原作：動物のお医者さん

アスキーアート：aahub
