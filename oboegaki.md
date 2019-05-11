# 覚書

## 構文解析

- パーサコンビネータは結局自前実装した
    - メリット
        - 準備終わったあととても楽
        - パース以外の処理を書かなくてよい
    - デメリット
        - 準備は面倒
            - Monad にするため Functor や Appricative の実装もしなければいけないため
- 型 vs 値の関係で表現
    - golang  は Interface vs Struct
    - Haskell では直和型で表現
    - 特定のデータを持ちたいが(例えば Identifire ) 型としてしか持てないなど( Expression )やや不便
- メンバ名を重複できないのがやっぱ不便
    - extensible を使うとか解決策はあるが