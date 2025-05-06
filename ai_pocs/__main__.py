import logging
import os
import shutil
from types import ModuleType
from typing import Dict, List

import dotenv
import gradio as gr

import ai_pocs.insight_care.insight_care as insight_care

MODULES = [insight_care]


def main():
    _setup_logging()
    dotenv.load_dotenv()
    _setup_working_dir()
    users = _load_users()

    with gr.Blocks(theme=gr.themes.Soft()) as demo:
        if users:
            gr.Button("Logout", link="/logout", size="sm")
        _load_modules(MODULES, demo, users)

    auth_users = [(user.username, user.password) for user in users.values()] if users else None
    demo.launch(server_name="0.0.0.0", server_port=8000, auth=auth_users, show_api=False)


def _setup_logging():
    logging.basicConfig(level=logging.WARNING,
                        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    logger = logging.getLogger("aipocs")
    logger.setLevel(logging.INFO)


def _setup_working_dir():
    working_dir = "var"
    if os.path.exists(working_dir):
        shutil.rmtree(working_dir)
    os.mkdir(working_dir)


class User:
    def __init__(self, username, password, group):
        self.username = username
        self.password = password
        self.group = group


def _load_users() -> Dict[str, User]:
    users_var = os.getenv("APP_USERS")
    if not users_var:
        return {}
    ret = {}
    for user in users_var.split(","):
        user_parts = user.split("@", 2)
        group = user_parts[1]
        user_parts = user_parts[0].split(":", 2)
        ret[user_parts[0]] = User(user_parts[0], user_parts[1], group)
    return ret


def _load_modules(modules: List[ModuleType], demo: gr.Blocks, users: Dict[str, User]):
    tabs = []
    for module in modules:
        tab = gr.Tab(label=_extract_module_name(module), visible=not users)
        with tab:
            module.build_gui()
        tabs.append(tab)
    if users:
        def _show_for_user(request: gr.Request):
            user = users.get(request.username)
            user_group = user.group if user else None
            return [gr.update(visible=user_group == _extract_module_name(m)) for m in modules]

        demo.load(_show_for_user, None, tabs)


def _extract_module_name(module: ModuleType) -> str:
    return module.__name__.split(".")[-1]


if __name__ == '__main__':
    main()
