# Haskey ![Haskell CI](https://github.com/kita127/haskey/workflows/Haskell%20CI/badge.svg)

## Description
haskey is a programming language written in Haskell.

It is a language that originally appeared in the book "Writing An Interpreter In Go".

It is originally written in Go.

## Rule
Go言語でつくるインタプリタ本と同じ条件とするため以下のルールとします

- base パッケージのみ使用する
- 例外で使用するパッケージ
    - text
        - 文字列データ用に使用
    - containers
        - Map 用に使用
    - raw-strings-qq
        - ユニットテスト時の生文字列の生成に使用
    - optparse-applicative
        - コマンドラインからの入力用に使用
    - HUnit
        - ユニットテストに使用
- Parsec は使わない
    - その他パーサコンビネータ系のライブラリは使用しない
- Alex、Happy などの字句解析、構文解析系のライブラリも使用しない

例外で使用する外部パッケージがありますが、このあたりは文字列型のパフォーマンスの向上や、
テスト時の手間を減らすことを目的に使用します。
プログラム言語作成に於いて本質的な部分には外部パッケージは使用していないつもりです。

## Synopsis

引数を与えない場合は REPL として起動します。

```
prompt$ haskey
Hello! This is the Haskey programming language!
Feel free to type in commands
Usage: haskey --help
>> let a = 100;
>> let b = 200;
>> a + b;
300
>> if(a > 1000){ true; }else{ false; };
False
>> let add = fn(x, y) { x + y };
>> let sub = fn(x, y) { x - y };
>> let applyFunc = fn(a, b, func) { func(a, b) };
>> applyFunc(2, 2, add);
4
>> applyFunc(10, 2, sub);
8
>> "Hello" + " " + "World";
Hello World
```

Haskey コードが書かれたファイルを指定して実行も可能です。

```
prompt$ cat example\sample01.haskey
let hoge = 100;
let fuga = 3;

let sum = fn(a, b){
    return a + b;
};

let mul = fn(a, b){
    return a * b;
};

let apply = fn(a, b, f){
    return f(a, b);
};

let resSum = apply(hoge, fuga, sum);
let resMul = apply(hoge, fuga, mul);

puts(resSum);
puts(resMul);

prompt$ haskey example\sample01.haskey
103
300


```

## Required
Stack

https://docs.haskellstack.org/en/stable/README/

## Installing

```
$ git clone https://github.com/kita127/haskey.git
$ cd ./haskey
$ stack build
$ stack install
```

## UnInstalling
`stack path --local-bin` で表示されるディレクトリにある haskey を削除してください

## Usage
FILE 引数には現在未対応です
```
Usage: haskey [FILE]
  Haskey is a programming language written in Haskell. If you don't enter any
  commands, repl runs.

Available options:
  -h,--help                Show this help text
  FILE                     input files
```

## Status

### リテラル
- 整数値
    - 100
    - -20
- 真偽値
    - true
    - false
- 文字列リテラル
    - "Hello World"
- NULL
    - null


### 演算
```
>> 100 + 20
120
>> 100 - 20
80
>> 100 * 20
2000
>> 100 / 20
5
>> true == true
True
>> true != true
False
>> 100 > 20
True
>> 100 < 20
False
>> "Hello" + " " + "World" 
Hello World
```

### 変数定義
```
>> let variable = 100;
>> variable;
100
>> let hoge = variable + 99;
>> hoge;
199

```

### if 式
```
>> if(100 > 20){ "OK"; };
OK
>> if(100 > 999){ "OK"; }else{ "NOT OK"; };
NOT OK
>> if(100 > 999){ "OK"; };
null
>> let res = if(true){ "hogehoge"; };
>> res
hogehoge
```

### 関数
```
>> let add = fn(x, y) { return x + y; };
>> let sub = fn(x, y) { return x - y; };
>> add(100, 20);
120
>> sub(100, 20);
80
>> fn(a, b){ a + " " + b }("love", "Haskell");
love Haskell
```

#### 高階関数
```
>> let applyFunc = fn(a, b, func) { return func(a, b); };
>> applyFunc(2, 2, add);
4
>> applyFunc(10, 2, sub);
8
```

#### クロージャ
```
>> let mulXFunc = fn(x){ fn(y){ y * x; } };
>> let doubleFunc = mulXFunc(2);
>> doubleFunc(100);
200
>> doubleFunc(200);
400
>> let threeTime = mulXFunc(3);
>> threeTime(100);
300
>> threeTime(200);
600
```

### 配列
```
>> let array = [1, 2, 3, 4];
>> array[2];
3
>> array[0] + array[3];
5
>> [5, 6, 7][1];
6
```

### 組み込み関数
```
>> let array = [1, 2, 3, 4];
>> len(array);
4
>> len("haskell");
7
>> first(array);
1
>> last(array);
4
>> rest(array);
[2, 3, 4]
>> push(array, "xyz");
[1, 2, 3, 4, xyz]
>> puts("hello world!");
hello world!
null
```

## Copyright
原作：動物のお医者さん

アスキーアート：aahub
