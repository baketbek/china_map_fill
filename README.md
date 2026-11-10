# china_map_fill

用 R 绘制带九段线的中国省级 / 市级填充地图（choropleth map）。

![省级填充图](output/province_fig_standard_color.svg)

## 效果

仓库 `output/` 下有四张矢量图：

| 文件 | 说明 |
|---|---|
| `province_fig_standard_color.svg` | 省级填充图 · 标准配色 |
| `province_fig_rmy_color.svg` | 省级填充图 · 红黄配色 |
| `city_fig_standard_color.svg` | 市级填充图 · 标准配色 |
| `city_fig_rmy_color.svg` | 市级填充图 · 红黄配色 |

全部为 SVG 矢量输出，可直接导入 Illustrator / Inkscape 继续加工。

## 数据准备

| 文件 | 用途 |
|---|---|
| `dataset/china_full_map.json` | 省级边界（含九段线） |
| `dataset/china_city_full_map.json` | 市级边界 |
| `dataset/data_prov.csv` | 省级指标数据（`prov` 省份代码、`casenum` 指标值） |
| `dataset/data_city.csv` | 市级指标数据 |
| `dataset/name_label.csv` | 名称与标签映射 |
| `dataset/centroid.csv` | 城市中心点坐标 |
| `dataset/PROV_name.xlsx` / `CITYNAMES.xlsx` | 省 / 市名称对照 |

## 运行

```r
# R 4.0+
install.packages(c("tidyverse", "sf", "fs", "RColorBrewer",
                   "hrbrthemes", "ggspatial"))

source("province_fill_map.R")   # 省级
source("city_fill_map.R")       # 市级
```

两个脚本各自独立，改 `read_csv()` 的数据源和 `write` 的输出路径即可复用。

## 关键实现

**边界数据**用 `sf::st_read()` 读 GeoJSON，从 `NAME` 字段截取前两位作为省份代码关联业务数据：

```r
cn <- st_read("dataset/china_full_map.json") %>%
      mutate(prov_id = str_sub(NAME, 1, 2))
```

**分级**用 `cut()` 按指标值切档，默认六档：

```r
breaks = c(0.0, 70.99, 139.99, 307.99, 499.99, 999.9, 100000)
labels = c("None", "71 - 139", "140 - 307", "308 - 599", "600 - 999", "> 1000")
```

改 `breaks` 和 `labels` 就能适配任意指标。

**配色**内置多套方案，脚本里直接切换变量名：

- `col_standard` — 紫红到橙的连续渐变
- `col_light` — 浅色系
- `col_white` — 带白色过渡
- `col_rmy` — 红黄经典配色

`常见图片配色方案推荐_中央美院.pdf` 是配色参考。

**九段线**包含在 `china_full_map.json` 的边界数据里，`sf` 读取后自动参与绘制，不需要额外图层。

## License

MIT
