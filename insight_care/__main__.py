import logging
import os
import shutil
from types import ModuleType
from typing import Dict, List

import dotenv
import gradio as gr

import insight_care.insight_care as insight_care

def main():
    _setup_logging()
    dotenv.load_dotenv()

    with gr.Blocks(theme=gr.themes.Soft()) as demo:
        insight_care.build_gui()
    demo.launch(server_name="0.0.0.0", server_port=8000, show_api=False)


def _setup_logging():
    logging.basicConfig(level=logging.WARNING,
                        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    logger = logging.getLogger("insight_care")
    logger.setLevel(logging.INFO)


if __name__ == '__main__':
    main()
