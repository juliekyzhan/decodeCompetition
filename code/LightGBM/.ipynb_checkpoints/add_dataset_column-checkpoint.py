import pandas as pd
import numpy as np

# 划分训练集和测试集
train_data, test_data = train_test_split(data, test_size=0.3, random_state=42)

# 创建数据集标识
dataset_labels = pd.Series(['train'] * len(train_data) + ['test'] * len(test_data), 
                          index=train_data.index.tolist() + test_data.index.tolist())

# 将标识按原始数据顺序排列
dataset_labels = dataset_labels.reindex(data.index)

analysis_outcomes = pd.concat([
    data_pred_merged[['full_name', 'issue','state_code', 'congress', 'age',
       'gender', 'party', 'chamber','seniority', 'ideology', 'district', 'committee_el','label', 'pred']].reset_index(drop=True),
    analysis_result[['importance_intl','importance_prof', 'importance_dom','importance_all']],
    pd.DataFrame({'dataset': dataset_labels.values})
], axis=1)

# 检查结果
print(f"analysis_outcomes 形状: {analysis_outcomes.shape}")
print("列名:")
print(analysis_outcomes.columns.tolist())
print("\ndataset 列的值分布:")
print(analysis_outcomes['dataset'].value_counts())