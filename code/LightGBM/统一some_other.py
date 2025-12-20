import json
import re


def normalize_ethnicity(json_file_path):
    # 读取JSON文件
    try:
        with open(json_file_path, 'r', encoding='utf-8') as file:
            data = json.load(file)

        # 检查数据是否为列表
        if not isinstance(data, list):
            print("JSON文件内容不是一个列表")
            return

        # 处理每个条目
        count = 0
        for item in data:
            # 检查是否有ethnicity字段且不为空
            if 'ethnicity' in item and item['ethnicity'] is not None:
                # 使用正则表达式查找并替换，不区分大小写
                # 先将字符串转换为Python列表
                try:
                    ethnicity_list = json.loads(item['ethnicity'])
                    updated = False
                    for i in range(len(ethnicity_list)):
                        # 检查是否匹配"Some other race"（不区分大小写）
                        if re.fullmatch(r'some other race', ethnicity_list[i], re.IGNORECASE):
                            ethnicity_list[i] = "Some Other Race"
                            updated = True
                            count += 1
                    if updated:
                        # 将列表转换回字符串
                        item['ethnicity'] = json.dumps(ethnicity_list)
                except json.JSONDecodeError:
                    print(f"无法解析ethnicity字段: {item['ethnicity']}")

        # 写回修改后的JSON文件
        with open(json_file_path, 'w', encoding='utf-8') as file:
            json.dump(data, file, ensure_ascii=False, indent=2)

        print(f"处理完成，共修改了{count}处'Some other race'为'Some Other Race'")

    except FileNotFoundError:
        print(f"错误：找不到文件 {json_file_path}")
    except Exception as e:
        print(f"处理过程中发生错误：{str(e)}")


if __name__ == "__main__":
    # 调用函数处理district.json文件
    normalize_ethnicity('district(统一some名称).json')
