import gradio as gr
from ai_pocs.insight_care import insight_care_prompts
from ai_pocs.insight_care.insight_care_states import IntroState


def build_gui():
    _, history = initialize_chat([])
    chatbot = gr.Chatbot(label="Asistente", value=history)
    chatbot.change(None, scroll_to_output=True, js="""
        function() {
            const chatbox = document.querySelector('[aria-label="chatbot conversation"]');
            if (chatbox) {
                chatbox.scrollTop = chatbox.scrollHeight;
            }
        }
        """)
    msg = gr.Textbox(label="Escribe tu mensaje:")
    msg.submit(respond, [msg, chatbot], [msg, chatbot])
    clear_button = gr.Button("Borrar Chat")
    clear_button.click(initialize_chat, [chatbot], [msg, chatbot])
    return chatbot, msg, clear_button


def initialize_chat(history):
    global session
    session = {
        "state": IntroState(),
        "messages": [],
        "ranking": {
            "WHO_1": None,
            "WHO_2": None,
            "WHO_3": None,
            "WHO_4": None,
            "WHO_5": None,
        },
    }
    session["messages"] = [
        {"role": "system", "content": insight_care_prompts.Prompts.SYSTEM_PROMPT.value}
    ]
    initial_message = session["state"].get_message(session)
    session["messages"].append({"role": "assistant", "content": initial_message})
    history = []
    history.append((None, initial_message))
    return "", history


def respond(message, history):
    global session
    session["messages"].append({"role": "user", "content": message})
    session["state"], session = session["state"].on_message(session)
    response = session["state"].get_message(session)
    session["messages"].append({"role": "assistant", "content": response})
    history.append((message, response))
    return "", history
