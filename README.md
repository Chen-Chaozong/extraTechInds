# extraTechInds
a R package provide some useful technical indicators

version 0.1.2

add two useful functions to calculate momentum indicators

Momentum : 用两列（或一列）价格数据计算动量，对第二列数据取一定滞后期数（可以为0），可先对价格数据取移动平均。

Momratio : 构建动量比率类指标，Momentum构建出动量后输入Momratio,可选择输出正动量比率、负动量比率、净动量比率。

设计思路在于扩展对动量的认识，例如：

定义正动量为当前价与n期内最低价的差: close - runMin(low,n)

定义负动量为n期内最高价与当前价的差: runMax(high,n)-close

正动量比率即为: [ close - runMin(low,n) ] / ( runMax(high,n)+runMin(low,n) )

实际上就是随机指标Stochastic Index
