import xml.etree.ElementTree as ET
import json
import sys

class AndroidManifestParser:
    def __init__(self, file_path):
        self.file_path = file_path
        self.tree = None
        self.root = None

    def load_xml(self):
        try:
            self.tree = ET.parse(self.file_path)
            self.root = self.tree.getroot()
            print(f"Successfully loaded {self.file_path}")
        except ET.ParseError as e:
            print(f"Error parsing XML: {e}")

# 从AndroidManifest.xml提取应用包名
    def get_package_name(self):
        if self.root is None:
            print("XML not loaded. Please call load_xml() first.")
            return None

        return self.root.get('package')
    
    def get_activity_data(self):
        if self.root is None:
            print("XML not loaded. Please call load_xml() first.")
            return None

        activities = self.root.findall(".//activity")
        activity_data = []

        for activity in activities:
            activity_info = {
                "name": activity.get("{http://schemas.android.com/apk/res/android}name"),
                "data": [],
                "isBrowsable": False,
                "exported": activity.get("{http://schemas.android.com/apk/res/android}exported"),
            }

            intent_filters = activity.findall("intent-filter")
            for intent_filter in intent_filters:
                
                categorys = intent_filter.findall("category")
                # print(categorys)
                for category in categorys:
                    if "android.intent.category.BROWSABLE" in category.get("{http://schemas.android.com/apk/res/android}name"):
                        activity_info["isBrowsable"] = True

                data_elements = intent_filter.findall("data")
                for data in data_elements:
                    scheme = data.get("{http://schemas.android.com/apk/res/android}scheme")
                    host = data.get("{http://schemas.android.com/apk/res/android}host")
                    path = data.get("{http://schemas.android.com/apk/res/android}path")
                    # TODO
                    pathPrefix = data.get("{http://schemas.android.com/apk/res/android}pathPrefix")
                    if scheme is None and host is None and path is None:
                        data_info = {}
                    else:
                        data_info = {
                            "scheme": data.get("{http://schemas.android.com/apk/res/android}scheme"),
                            "host": data.get("{http://schemas.android.com/apk/res/android}host"),
                            "path": data.get("{http://schemas.android.com/apk/res/android}path")
                        }
                    activity_info["data"].append(data_info)
            # if len(activity_info["data"]) == 0:
            #     continue
            activity_data.append(activity_info)

        return activity_data


    def filter_by_key_and_value(self, kargs):
        if self.root is None:
            print("XML not loaded. Please call load_xml() first.")
            return None
        

    def save_to_json(self, data, output_file):
        try:
            with open(output_file, 'w', encoding='utf-8') as f:
                json.dump(data, f, ensure_ascii=False, indent=4)
            print(f"Data successfully saved to {output_file}")
        except IOError as e:
            print(f"Error saving to JSON: {e}")

if __name__ == "__main__":
    file_path = sys.argv[1]
    output_file = sys.argv[2]

    parser = AndroidManifestParser(file_path)
    parser.load_xml()
    activity_data = parser.get_activity_data()
    
    if activity_data:
        parser.save_to_json(activity_data, output_file)
