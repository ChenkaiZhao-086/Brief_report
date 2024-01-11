import pandas as pd
import datetime as dtm

CurDat = dtm.datetime.now()
data = pd.read_json("/Users/faust/Downloads/perCountryData.json")

# Extracting the nested 'regions' column
regions_data = pd.json_normalize(data['regions'])
# 用于将嵌套的JSON数据结构（例如列表、字典等）平展成表格形式的DataFrame

# Extracting the 'distributions' column
distributions_data = pd.json_normalize(
    regions_data['distributions'].explode(), # explode将每个列表元素转换成单独的行。假设distributions列中的每个单元格都包含了一个列表，explode()会将这个列表中的每个元素转化为DataFrame的新行
    record_path='distribution', 
    meta=['country'] # meta 参数用于指定需要保留的外层信息。
)

# distributions_data['country'].unique()
filtered_data = distributions_data[~distributions_data['country'].str.startswith("Region")].fillna(0)

MeltDat = filtered_data.melt(id_vars=['total_sequences', 'week', 'country'], var_name='type', value_name='cases')
MeltDat['week'] = pd.to_datetime(MeltDat['week'])

# filter in one year
MeltDat = MeltDat[MeltDat['week'] >= CurDat - dtm.timedelta(days=365)]

MeltDat = (MeltDat.assign(year=lambda df: df['week'].dt.year,
           month=lambda df: df['week'].dt.month))
MeltDat['TotalYear'] = MeltDat.groupby(['year', 'country'])['cases'].transform('sum')
MeltDat['PercYear'] = MeltDat['cases'] / MeltDat['TotalYear']
MeltDat['percDay'] = MeltDat['cases'] / MeltDat['total_sequences']

MeltDat.to_csv("json.csv", index=False)