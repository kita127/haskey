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
FILE 引数には現在未対応です
```
Usage: haskey [FILE]
  Haskey is a programming language written in Haskell. If you don't any
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



## Examples
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


## Copyright
原作：動物のお医者さん

アスキーアート：aahub
