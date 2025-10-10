#### Lab of Big Data for Social Sciences ####

# Author: Riccardo Omenti
# e-mail: riccardo.omenti2@unibo.it


#############################

# clean environment

rm(list=ls())

#### Install Packages #####

#install.packages('ggthemes') 
#install.packages('RColorBrewer') 
#install.packages('moderndive') 
#install.packages('ggstats')
library('RColorBrewer') 
# various qualitative color palettes 
library('ggthemes') 
# various themes in ggplot 
library("tidyverse")  
# linear regression in tidyverse
library("moderndive")
# tables 
library('knitr')
# plot coefficients
library('ggstats')


#### Upload data ####

# Violence data
gpi_data <- read.csv('Data/data_gpi_final.csv') 
# Data on male life expectancy
life_exp_male <- read.csv('Data/male_data.csv')
# Data on female life expectancy
life_exp_female <- read.csv('Data/female_data.csv')

# コメントアウトショートカットキー: ctrl + shift + c

# Combine data sets
# 1. GPIを基準にして始める
# 2. 男性寿命データ (life_exp_male) を、iso3（国コード）と Year（年）という共通カラムを使って結合する
# 3. 次に、その結果に 女性寿命データ (life_exp_female) を、iso3, Year, country の3つの共通カラムで結合する
# 最終的に、すべての情報がそろった新しいデータフレームを data に保存する

data <- gpi_data |>
  inner_join(life_exp_male,by=c("iso3","Year")) |>
  inner_join(life_exp_female,by=c("iso3","Year","country")) 

#### Question 1 ####

# data to be plotted

# Step 1: 結合済みデータ "data" をもとに処理を開始
# Step 2: "Year"（年）と "area"（地域）ごとにグループ化する
#         → 各年・各地域単位で集計を行うため
# Step 3: summarize() 関数で「人口加重平均（weighted mean）」を計算する
#         - gpi = 暴力レベル（Global Peace Index）
#         - pop = 各国の人口（重みとして使用）
#         - weighted.mean() = 人口を重みとして平均を計算する関数
#         - na.rm = FALSE は欠損値を無視しない設定（TRUEにすると無視）

data_violence_region <- data |>
  group_by(Year,area) |>
  summarize(gpi=weighted.mean(x=gpi,w=pop,na.rm = FALSE)) 

# plot

# --- 地域ごとの暴力レベル（GPI）の推移を可視化するプロットを作成 ---

# ggplot() 関数を使って折れ線グラフを作成する。
# データフレーム "data_violence_region" には、
# 各地域（area）ごとの年（Year）別GPIの平均値が入っている。

plot1 <- ggplot(
  data = data_violence_region,                 # 使用するデータ
  mapping = aes(x = Year, y = gpi, color = area) # X軸=年, Y軸=GPI, 色=地域
) +
  geom_line() +        # 各地域のGPIの推移を線で描く
  geom_point() +       # 各年ごとに点を追加して視認性を上げる
  theme_clean() +      # 背景をシンプルな白基調に変更
  scale_color_brewer(
    name = "Region",   # 凡例タイトル
    palette = "Set3"   # RColorBrewerの「Set3」カラーパレットを使用 (https://r-graph-gallery.com/38-rcolorbrewers-palettes.html)
  ) +
  scale_x_continuous(
    breaks = seq(2009, 2023, 2),  # X軸の目盛り（2009年から2年刻み）
    labels = seq(2009, 2023, 2)   # 表示ラベルも同じ範囲で指定
  ) +
  xlab('Year') +                  # X軸タイトルを「Year」に設定
  ylab('Level of Violence') +     # Y軸タイトルを「Level of Violence」に設定
  ggtitle('Region-specific Violence over Time')  # グラフタイトル

# 最後にプロットを出力
plot1

# 出力結果: 各地域ごとの暴力レベル（GPI）の平均値を、2009年〜2023年の期間で折れ線グラフとして表示。
# 線の色で地域を区別し、暴力レベルの時間的な変化を比較できる。



#### Question 2 ####

# --- 地域ごとの男女別平均寿命の推移を作成・可視化 ---

# ▼ Step 1: データ整形 -----------------------------------------------------

# "data" には男性・女性の寿命データが別々の列（wide形式）で入っている。
# そこで、pivot_longer() を使って「long形式（縦型）」に変換する。
# つまり「sex（性別）」という1つの列にまとめ、対応する寿命値を「life_exp」として格納する。

data_life_exp_region <- data |>
  pivot_longer(
    !c('Year','iso3','pop','area','gpi','country'),  # 除外する列（それ以外の列を変換対象にする）
    names_to = 'sex',       # 元の列名を 'sex' という新しい列に格納
    values_to = 'life_exp'  # 対応する値を 'life_exp' 列に格納
  ) |>
  
  # ▼ Step 2: 性別変数を整形
  # もとの列名（例："life_exp_male", "life_exp_female"）の10文字目以降を抽出して
  # "male" と "female" というシンプルなラベルを作成する。
  mutate(sex = substr(sex, 10, length(sex))) |>
  
  # ▼ Step 3: 地域・年・性別ごとにグループ化して加重平均を計算
  group_by(Year, area, sex) |>
  summarize(
    life_exp = weighted.mean(x = life_exp, w = pop, na.rm = FALSE)  # 人口加重平均, na.rm = 欠損値（NA）を削除せずに計算する設定（必要に応じてTRUEに変更可能）
  )

# 結果: 各地域・各年・性別ごとの平均寿命データが整う。
# これをもとに、次でグラフを作成する。


# ▼ Step 4: 男女別・地域別の平均寿命の推移を可視化 --------------------

plot2 <- ggplot(
  data = data_life_exp_region,                     # データ入力
  mapping = aes(x = Year, y = life_exp, color = area)  # X軸=年, Y軸=平均寿命, 色=地域
) +
  geom_line() +      # 地域ごとの寿命推移を線で表示
  geom_point() +     # 各年のデータ点を追加
  annotate("text", x = 2020, y = 55, label = "COVID-19") +  # COVID-19の影響を示す注釈を追加
  theme_clean() +    # 背景をシンプルに
  facet_wrap(~sex) + # 性別ごとに2つのグラフを作成（男女比較ができる）
  xlab('Year') +     # X軸ラベル
  ylab('Life Expectancy at Age 30') +  # Y軸ラベル
  scale_color_brewer(name = "Region", palette = "Set1") +  # カラーパレット設定
  ggtitle('Region-specific Life Expectancy over Time')     # グラフタイトル

# プロットを出力
plot2

# 出力結果:
# ・X軸 = 年（2009〜2023）
# ・Y軸 = 30歳時点の平均余命（Life Expectancy at Age 30）
# ・色 = 地域（area）
# ・2つのファセット（facet_wrap）で、男性と女性の平均寿命の推移を並べて比較できる。
# ・COVID-19の影響を2020年に注釈として追加。


#### Question 3 ####
# --- 暴力レベル（GPI）と平均寿命の関係を2023年データで可視化する準備とプロット ---

# ▼ Step 1: データ整形 -----------------------------------------------------

# "data" には男女別の寿命データが wide 形式で入っているため、
# pivot_longer() を使って long 形式（縦型）に変換する。
# これにより「sex（性別）」列と「life_exp（寿命）」列を作る。

data_scatter <- data |>
  pivot_longer(
    !c('Year','iso3','pop','area','gpi','country'), # 除外する列を指定
    names_to = 'sex',        # 元の列名（例: life_exp_male）を sex に格納
    values_to = 'life_exp'   # 各列の値を life_exp に格納
  ) |>
  
  # ▼ Step 2: 性別情報の整理
  # 列名（"life_exp_male" や "life_exp_female"）の10文字目以降を取り出して
  # "male" と "female" だけを残す。
  mutate(sex = substr(sex, 10, length(sex))) |>
  
  # ▼ Step 3: 2023年のデータだけを抽出
  filter(Year == 2023)

# 結果: 各国・地域・性別ごとに2023年の暴力レベル（gpi）と平均寿命（life_exp）が含まれる。
# これを使って散布図を作成する。


# ▼ Step 4: 暴力レベルと寿命の関係をプロット ------------------------------

plot3 <- ggplot(
  data = data_scatter,                       # データ入力
  mapping = aes(x = gpi, y = life_exp)       # X軸 = 暴力レベル, Y軸 = 平均寿命
) +
  geom_point(size = 2) +                     # 各国を点で表示（点の大きさ=2）
  xlab('Level of Violence') +                # X軸ラベル
  ylab('Life Expectancy at Age 30') +        # Y軸ラベル
  theme_economist() +                        # 経済誌風のテーマを使用（背景をグレーに）
  
  # ▼ Step 5: 回帰直線を追加
  # method = "lm" は線形回帰を意味し、se = FALSE で信頼区間を非表示に。
  # color = 'red' で回帰線を赤に設定。
  geom_smooth(method = "lm", se = FALSE, color = 'red') +
  
  # ▼ Step 6: 性別ごとにプロットを分ける
  facet_wrap(~sex) +                         # 男性・女性でサブプロットを分ける
  
  # ▼ Step 7: 軸とタイトルのフォントサイズ・太字設定
  theme(
    axis.text.y   = element_text(size = 15, face = "bold"),   # Y軸目盛り
    axis.title.y  = element_text(size = 15, face = "bold"),   # Y軸タイトル
    axis.text.x   = element_text(size = 15, face = "bold"),   # X軸目盛り
    axis.title.x  = element_text(size = 15, face = "bold")    # X軸タイトル
  )

# プロットを出力
plot3

# 出力結果:
# ・X軸 = 暴力レベル（GPI値が高いほど暴力的）
# ・Y軸 = 平均寿命（30歳時点の予測余命）
# ・赤い線 = 線形回帰直線（暴力レベルと寿命の相関を示す）
# ・facet_wrap(~sex) により、男性と女性の散布図が別々に表示される
# → 男女の傾向を比較できる。


#### Question 4 ####

# --- 最も暴力的な国と最も平和的な国の比較（男性寿命の分布を箱ひげ図で可視化） ---

# ▼ Step 1: 暴力レベルが高い国（2023年）を抽出 ------------------------

data_most_violence <- data |>
  filter(Year == 2023) |>     # 2023年のデータだけに絞る
  slice_max(gpi, n = 20) |>   # gpi（暴力レベル）が最も高い上位20か国を抽出
  mutate(label = 'Most Violent')  # 後で識別できるようにラベル列を追加

# 🔍 slice_max() の説明：
#   - 「指定した列の値が大きい順」に並べて、上から n 件を抽出する関数。
#   - ここでは gpi が高い国（暴力的な国）を選んでいる。
#   - 似た関数に slice_min()（値が小さい順に n 件抽出）がある。

# 例）slice_max(gpi, n=3)
#     gpiが一番高い国から順に3か国を取得。


# ▼ Step 2: 暴力レベルが低い国（2022年）を抽出 ------------------------

data_most_peaceful <- data |>
  filter(Year == 2022) |>     # 2022年のデータに絞る
  slice_min(gpi, n = 20) |>   # gpi（暴力レベル）が最も低い上位20か国を抽出
  mutate(label = 'Most Peaceful')  # 平和な国としてラベルを追加

# 🔍 slice_min() の説明：
#   - 指定した列の値が「最も小さい」ものを n 件取り出す。
#   - 今回は gpi が低い（＝平和な）国を抽出している。


# ▼ Step 3: 2つのデータを1つに結合 ------------------------------------

data_plot <- rbind(data_most_violence, data_most_peaceful)
# rbind() = 「行方向に結合」する関数。
# "Most Violent" と "Most Peaceful" のデータを上下にくっつけて1つにまとめる。

# 結果: data_plot は次のような構造になる
# | country | Year | gpi | life_exp_male | label          |
# |----------|------|-----|---------------|----------------|
# | Syria    | 2023 | 3.9 | 50.2          | Most Violent   |
# | ...      | ...  | ... | ...           | ...            |
# | Norway   | 2022 | 1.2 | 82.5          | Most Peaceful  |
# | ...      | ...  | ... | ...           | ...            |


# ▼ Step 4: 箱ひげ図を作成 ---------------------------------------------

plot4 <- ggplot(
  data = data_plot,                                # データ入力
  mapping = aes(x = label, y = life_exp_male, fill = label)  # X=ラベル, Y=男性寿命
) +
  geom_boxplot() +                                 # 箱ひげ図を描く
  coord_cartesian(ylim = c(30, 60)) +              # Y軸の範囲を30〜60に固定
  scale_y_continuous(
    breaks = seq(10, 60, 10),                      # 10刻みで目盛りを設定
    labels = seq(10, 60, 10)
  ) +
  theme_tufte() +                                  # ミニマルデザインのテーマを適用
  scale_fill_discrete(name = '') +                 # 凡例タイトルを非表示
  xlab('') +                                       # X軸タイトルなし
  ylab('Life Expectancy at Age 30') +              # Y軸タイトル
  theme(
    axis.text.y = element_text(size = 15, face = "bold"),   # Y軸の数字
    legend.text = element_text(size = 15, face = "bold"),   # 凡例テキスト
    axis.title.y = element_text(size = 15, face = "bold"),  # Y軸タイトル
    axis.text.x = element_text(size = 15, face = "bold"),   # X軸ラベル
    axis.title.x = element_text(size = 15, face = "bold")   # X軸タイトル（今回は空）
  )

# プロットを表示
plot4

# 出力結果:
#   ・X軸に「Most Violent」「Most Peaceful」の2カテゴリー
#   ・Y軸に「男性の平均寿命（30歳時点）」を表示
#   ・箱ひげ図（Boxplot）で、それぞれの分布を比較
#   → 暴力的な国ほど平均寿命が低く、分散が大きい傾向が視覚的にわかる。

#### Question 5 ####

# --- 最も暴力的な国と最も平和的な国の男性寿命の分布を密度プロットで比較 ---

# ▼ Step 1: ggplot の基本設定 --------------------------------------------
plot5 <- ggplot(
  data = data_plot,                               # 使用するデータ（Most Violent + Most Peaceful）
  aes(x = life_exp_male, color = label, fill = label) # X軸=男性寿命, 色と塗りでグループを区別
) +
  
  # ▼ Step 2: 密度プロット（density plot）を作成 -------------------------
geom_density(alpha = 0.2, na.rm = TRUE) +
  # geom_density() = 連続データの分布を滑らかな曲線で表す
  # alpha = 透過度（0.2で少し透明にして重なりを見やすくする）
  # na.rm = TRUE で欠損値を無視して描画する
  
  # ▼ Step 3: 背景テーマの設定 ------------------------------------------
theme_stata() +
  # Stata風（経済学・社会科学風）のテーマを適用
  
  # ▼ Step 4: 凡例（legend）の設定 --------------------------------------
scale_fill_discrete(name = '') +    # 凡例のタイトルを非表示（fill）
  scale_color_discrete(name = '') +   # 凡例のタイトルを非表示（color）
  
  # ▼ Step 5: 軸の調整 ---------------------------------------------------
coord_cartesian(xlim = c(30, 55)) +  # X軸（寿命）の範囲を30〜55に制限
  scale_x_continuous(
    breaks = seq(30, 55, 5),          # 5刻みで目盛りを設定
    labels = seq(30, 55, 5)
  ) +
  
  # ▼ Step 6: 軸ラベル ---------------------------------------------------
ylab('Density') +                   # Y軸タイトルを「Density」に設定（分布の密度）
  xlab('Life Expectancy') +           # X軸タイトルを「Life Expectancy」に設定
  
  # ▼ Step 7: テーマ調整（文字の大きさや配置など） -----------------------
theme(
  axis.text.y   = element_text(size = 15, face = "bold"),   # Y軸の目盛り
  legend.position = "bottom",                              # 凡例を下に配置
  axis.title.y  = element_text(size = 15, face = "bold"),   # Y軸タイトル
  axis.text.x   = element_text(size = 15, face = "bold"),   # X軸の目盛り
  axis.title.x  = element_text(size = 15, face = "bold")    # X軸タイトル
)

# ▼ Step 8: プロットを出力 -----------------------------------------------
plot5

# 出力結果:
# ・X軸 = 男性の平均寿命（30〜55歳の範囲）
# ・Y軸 = 密度（Density）＝その範囲に国が分布する相対的な割合
# ・色と塗り = 「Most Violent（暴力的な国）」と「Most Peaceful（平和な国）」を区別
# ・2つの分布がどのあたりに重なるか、寿命の偏りを視覚的に確認できる。


#### Question 6 ####

# --- 暴力レベル（GPI）、平均寿命、人口、地域の関係を多次元的に可視化（散布図） ---

plot6 <- ggplot(
  data = data_scatter,                             # 使用するデータ（2023年データ, 男女別）
  mapping = aes(x = gpi, y = life_exp)             # X軸 = 暴力レベル, Y軸 = 平均寿命
) +
  
  # ▼ Step 1: 散布図を作成
  geom_point(aes(
    shape = area,                                  # 地域ごとに点の形を変える
    color = area,                                  # 地域ごとに色を変える
    size = pop                                     # 点の大きさで人口規模を表す
  )) +
  
  # ▼ Step 2: 軸ラベルを設定
  xlab('Level of Violence') +                      # X軸タイトル
  ylab('Life Expectancy at Age 30') +              # Y軸タイトル
  
  # ▼ Step 3: 背景テーマ
  theme_minimal() +                                # シンプルな白背景のテーマを使用
  
  # ▼ Step 4: 回帰直線を追加
  geom_smooth(method = "lm", se = FALSE, color = 'red') + 
  # method = "lm" → 線形回帰直線を描画
  # se = FALSE → 信頼区間を非表示
  # color = 'red' → 回帰線を赤で描く
  
  # ▼ Step 5: 凡例（legend）のタイトル設定
  labs(size = 'Population Size') +                 # 点のサイズ凡例のタイトルを変更
  
  # ▼ Step 6: 点の形と色の設定
  scale_shape_manual(
    name = "Area",                                 # 凡例タイトル（地域名）
    values = c(16, 17, 15, 3, 4, 8, 7, 9)          # 地域ごとに異なる点の形（最大8種類）
  ) +
  scale_color_brewer(
    name = "Area",                                 # 凡例タイトル
    palette = "Dark2"                              # RColorBrewerのDark2カラーパレットを使用
  ) +
  
  # ▼ Step 7: Y軸（平均寿命）の目盛り設定
  scale_y_continuous(
    breaks = seq(30, 55, 5),                       # 5刻みで目盛り設定
    labels = seq(30, 55, 5)
  ) +
  
  # ▼ Step 8: 性別ごとにプロットを分割（facet）
  facet_wrap(~sex) +                               # 男性・女性別にサブプロットを作成
  
  # ▼ Step 9: テキスト・フォント・凡例デザインの微調整
  theme(
    axis.text.y    = element_text(size = 12, face = "bold"),  # Y軸目盛り
    axis.title.y   = element_text(size = 15, face = "bold"),  # Y軸タイトル
    legend.title   = element_text(size = 15, face = "bold"),  # 凡例タイトル
    legend.text    = element_text(size = 15, face = "bold"),  # 凡例テキスト
    axis.text.x    = element_text(size = 12, face = "bold"),  # X軸目盛り
    axis.title.x   = element_text(size = 15, face = "bold")   # X軸タイトル
  )

# プロットを出力
plot6

# 出力結果:
# ・X軸 = 暴力レベル（GPI）
# ・Y軸 = 平均寿命（Life Expectancy at Age 30）
# ・点の色と形 = 地域（area）
# ・点の大きさ = 人口（pop）
# ・赤い線 = 線形回帰直線（暴力レベルと寿命の関係）
# ・facet_wrap(~sex) により、男女で別々のプロットを表示


#### Question 7 ####

# --- 2008年から2023年の間で暴力レベルが増加／減少した国を分析・可視化 ---

# ▼ Step 1: 暴力が「最も増加した」国を抽出 -----------------------------

data_high <- data |>
  filter(Year %in% c(2008, 2023)) |>          # 2008年と2023年のみを抽出
  select(Year, country, area, gpi) |>         # 必要な列のみ残す
  pivot_wider(
    names_from = 'Year',                      # "Year"列の値を新しい列名に
    values_from = 'gpi'                       # 各年のGPI値を列として展開
  ) |>
  mutate(
    var = (`2023` - `2008`) / `2008`          # GPIの変化率（2008→2023）を計算
  ) |>
  group_by(area) |>                           # 地域ごとにグループ化
  slice_max(var, n = 5) |>                    # GPIの増加率が最も大きい国を上位5件抽出
  mutate(label = 'Highest Violence Increase') # ラベルを追加（凡例用）

# 🔍 説明：
# - pivot_wider() は「long形式 → wide形式」に変換
#   例：Year列の2008と2023を、それぞれの列として展開。
# - var は変化率：
#   (2023年のGPI − 2008年のGPI) ÷ 2008年のGPI
# - slice_max(var, n=5) は、「最も増加した」上位5か国を選ぶ。


# ▼ Step 2: 暴力が「最も減少した」国を抽出 -----------------------------

data_low <- data |>
  filter(Year %in% c(2008, 2023)) |>          # 同じく2008年と2023年のみ使用
  select(Year, country, area, gpi) |>
  pivot_wider(names_from = 'Year', values_from = 'gpi') |>
  mutate(
    var = (`2023` - `2008`) / `2008`          # GPIの変化率（同様に計算）
  ) |>
  group_by(area) |>
  slice_min(var, n = 5) |>                    # GPIの変化率が最も小さい（＝減少した）国5件
  mutate(label = 'Highest Violence Decrease') # ラベルを追加

# 🔍 slice_min(var, n=5)
# - 変化率（var）が最も低い国、つまり暴力が減った国を抽出。
# - 「平和化が進んだ国」と解釈できる。


# ▼ Step 3: 2つのデータを結合 --------------------------------------------

plot_data <- rbind(data_high, data_low)
# rbind() = 行方向にデータを結合（上下にくっつける）


# ▼ Step 4: 特定地域（例：北アフリカ・西アジア）だけを抽出 --------------

plot7 <- plot_data |>
  filter(area == 'Northern Africa and Western Asia') |>  # 地域を限定
  ggplot(
    aes(
      x = reorder(country, var),   # 変化率で国名を並び替え
      y = var,                     # Y軸 = 変化率
      fill = label                 # 塗り = 増加 or 減少
    )
  ) +
  
  geom_bar(stat = 'identity') +               # 棒グラフ（値をそのまま高さに使用）
  theme_clean() +                             # シンプルな背景テーマ
  xlab('Country') +                           # X軸タイトル
  ylab('Variation in Violence') +             # Y軸タイトル
  scale_fill_brewer(name = '', palette = 'Set2') +  # カラーパレット（Set2）
  scale_y_continuous(labels = scales::percent) +    # Y軸をパーセント表示
  
  # ▼ Step 5: テーマ設定（フォントや凡例の位置など）
  theme(
    axis.text.x = element_text(
      angle = 45, vjust = 1, hjust = 1,
      size = 12, face = "bold"
    ),
    legend.position = 'bottom',                # 凡例を下に配置
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15, face = "bold")
  )

# プロットを表示
plot7


# ▼ Step 6: グラフをPDFとして保存 -----------------------------------------

ggsave(
  filename = "Results/plot7.pdf",  # 保存先ファイル名
  plot = plot7,                    # 保存するプロット
  height = 20,                     # 高さ（cm）
  width = 40,                      # 幅（cm）
  units = "cm",                    # 単位をcmに設定
  dpi = 400                        # 解像度（高画質）
)

# 出力結果:
# - 北アフリカ・西アジア地域の中で、
#   2008年から2023年にかけて暴力が最も「増加」または「減少」した上位5か国を棒グラフで表示。
# - 棒の高さ：変化率（var）
# - 色：増加（Highest Violence Increase）と減少（Highest Violence Decrease）の区別
# - Y軸はパーセント表示で変化の大きさを直感的に理解できる。


#### Question 8 ####

# make sure to pick a reference category 
# read categorical variables as factors

# --- 参照カテゴリを設定し、重み付き線形回帰を推定、結果を表と図で確認する ---

# ▼ Step 1: 因子化と参照カテゴリの指定
#   - sex と area を factor に変換し、基準となる参照カテゴリを指定する
#   - relevel(ref = ...) で基準カテゴリを設定
#   - 基準は推定結果の解釈の土台になるため、分析意図に合わせて選ぶ
data_scatter <- data_scatter |>
  mutate(
    sex  = relevel(as.factor(sex),  ref = "female"),              # 性別の基準を女性に
    area = relevel(as.factor(area), ref = "Sub-Saharan Africa")   # 地域の基準をサブサハラ・アフリカに
  )

# ▼ Step 2: 重み付き線形回帰モデルの推定
#   - 目的変数: life_exp（30歳時点の平均余命）
#   - 説明変数: gpi（暴力レベル）, sex（男女ダミー）, area（地域ダミー）
#   - weights = pop で人口を重みとして使用
#     大きな人口を持つ国の情報をより反映させるため
model <- lm(
  life_exp ~ gpi + sex + area,
  data    = data_scatter,
  weights = pop
)

summary(model)

# ▼ Step 3: 回帰表を表示
#   - moderndive::get_regression_table() は見やすい要約表を返す
#   - 係数, 標準誤差, t値, p値, 信頼区間などを確認できる
get_regression_table(model)

# ▼ Step 4: 回帰係数を視覚化
#   - ggstats::ggcoef_model() は係数と信頼区間をプロットで表示
#   - 効果の大きさと有意性を直感的に比較できる
ggcoef_model(model)


#-------自分用-------
plot8 <- ggcoef_model(model)

plot8

ggsave(
  filename = "Results/plot8_regression_coefficients.pdf",  # 保存ファイル名
  plot = plot8,      # 保存するオブジェクト（ここで指定！）
  height = 20,       # 高さ（cm）
  width = 30,        # 幅（cm）
  units = "cm",      # 単位をcmに設定
  dpi = 400          # 解像度（高画質）
)

