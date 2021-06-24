# 原始碼說明文件
- ``data/``: 爬取下來之資料，分為工作內容及條件要求兩部分。
- ``data_analyzing/``: 將資料以各個類別進行處理，詳細請見其內之[README.md](data_analyzing/README.md)
- ``requirement_vectorizing/``: 將條件要求各類別敘述中的物件整理為一個向量。
- ``shiny/``: 將整理好的圖表透過shiny app顯示出來，詳細請見其內之[README.md](shiny/README.md)
- ``webscraping/``: 將104求職網上的各類別工作資訊以爬蟲套件爬取下來。