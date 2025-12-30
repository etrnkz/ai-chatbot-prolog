import json
from datetime import datetime
from typing import List, Tuple

import requests
import streamlit as st

st.set_page_config(
    page_title="Logic Chat",
    page_icon=":speech_balloon:",
    layout="wide",
    initial_sidebar_state="expanded",
)

st.markdown(
    """
    <style>
        @import url('https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;600&family=Sora:wght@500;700&display=swap');

        :root {
            --bg-top: #060b1b;
            --bg-mid: #0d152f;
            --bg-bottom: #0f1e3c;
            --card: rgba(20, 27, 54, 0.85);
            --accent: #00d6a3;
            --accent-2: #f5a524;
            --text: #e8ecf3;
            --muted: #a8b3c7;
            --border: rgba(255, 255, 255, 0.08);
        }

        .stApp {
            background: radial-gradient(circle at 10% 20%, var(--bg-mid), transparent 25%),
                        radial-gradient(circle at 80% 0%, #0e233f, transparent 28%),
                        linear-gradient(135deg, var(--bg-top), var(--bg-mid) 40%, var(--bg-bottom));
            color: var(--text);
        }

        .block-container {
            padding-top: 1.5rem;
            padding-bottom: 2rem;
        }

        h1, h2, h3, h4, h5, h6 {
            font-family: 'Sora', sans-serif;
            letter-spacing: -0.03em;
        }

        .main-subtitle {
            color: var(--muted);
            margin-bottom: 1rem;
        }

        .metric-card {
            border: 1px solid var(--border);
            background: var(--card);
            padding: 0.9rem 1.1rem;
            border-radius: 14px;
        }

        .toolbar button, .sidebar button {
            border-radius: 10px;
        }

        .stChatMessage [data-testid="stMarkdownContainer"] {
            color: var(--text);
        }

        .stChatMessage {
            background: var(--card);
            border: 1px solid var(--border);
            border-radius: 14px;
            padding: 0.8rem;
            margin-bottom: 0.4rem;
        }

        .assistant-msg {
            border-left: 3px solid var(--accent);
        }

        .user-msg {
            border-left: 3px solid var(--accent-2);
        }

        .timestamp {
            color: var(--muted);
            font-size: 0.85rem;
            margin-top: 0.25rem;
        }

        .hint-box {
            border: 1px dashed var(--border);
            background: rgba(255, 255, 255, 0.03);
            padding: 0.9rem 1rem;
            border-radius: 12px;
            color: var(--muted);
        }
    </style>
    """,
    unsafe_allow_html=True,
)

DEFAULT_API = "http://localhost:8000"


def init_state() -> None:
    if "messages" not in st.session_state:
        st.session_state.messages = []
    if "api_url" not in st.session_state:
        st.session_state.api_url = DEFAULT_API
    if "last_error" not in st.session_state:
        st.session_state.last_error = None


def append_message(role: str, content: str) -> None:
    stamp = datetime.now().strftime("%H:%M:%S")
    st.session_state.messages.append({"role": role, "content": content, "time": stamp})


def build_payload(
    message: str, model: str, temperature: float, max_tokens: int
) -> dict:
    return {
        "message": message,
        "model": model,
        "temperature": temperature,
        "max_tokens": max_tokens,
    }


def post_message(api_base: str, payload: dict) -> Tuple[str | None, str | None]:
    url = api_base.rstrip("/") + "/chat"
    try:
        response = requests.post(url, json=payload, timeout=40)
        response.raise_for_status()
        data = response.json()
        return data.get("reply", "No response from AI."), None
    except requests.exceptions.RequestException as exc:
        return None, f"Request failed: {exc}"
    except json.JSONDecodeError:
        return None, "Backend returned non-JSON payload."


init_state()

with st.sidebar:
    st.subheader("Controls")
    st.session_state.api_url = st.text_input(
        "Prolog API base",
        value=st.session_state.api_url,
        help="Default matches the local server at port 8000.",
    )
    model = st.selectbox(
        "Model label", ["gpt-3.5-turbo", "gpt-4", "gpt-4-turbo"], index=1
    )
    temperature = st.slider("Temperature", 0.0, 1.0, 0.7, 0.05)
    max_tokens = st.slider("Max tokens", 64, 2048, 512, 32)

    col_a, col_b = st.columns(2)
    with col_a:
        if st.button("Clear chat", use_container_width=True):
            st.session_state.messages = []
            st.session_state.last_error = None
            st.rerun()
    with col_b:
        if st.button("Test backend", use_container_width=True):
            ping_payload = build_payload("ping", model, temperature, max_tokens)
            _, error = post_message(st.session_state.api_url, ping_payload)
            if error:
                st.session_state.last_error = error
                st.warning(error)
            else:
                st.session_state.last_error = None
                st.success("Backend reachable.")

    if st.session_state.messages:
        transcript = "\n".join(
            [
                f"[{m['time']}] {m['role']}: {m['content']}"
                for m in st.session_state.messages
            ]
        )
        st.download_button(
            label="Download chat as text",
            data=transcript,
            file_name="chat_log.txt",
            mime="text/plain",
            use_container_width=True,
        )

st.title("Logic Chat")
st.markdown(
    """
    <p class="main-subtitle">Prolog handles the reasoning. Streamlit handles the experience.</p>
    """,
    unsafe_allow_html=True,
)

hint_cols = st.columns([3, 2])
with hint_cols[0]:
    st.markdown(
        """
        <div class="hint-box">
        Keep the Prolog server running (`swipl server.pl`).
        Ask for greetings, riddles, books, or any pattern covered in Data.json.
        </div>
        """,
        unsafe_allow_html=True,
    )
with hint_cols[1]:
    st.markdown(
        """
        <div class="metric-card">
            <strong>Session</strong><br>
            Messages: {count}
        </div>
        """.format(
            count=len(st.session_state.messages)
        ),
        unsafe_allow_html=True,
    )

chat_panel = st.container()
with chat_panel:
    for msg in st.session_state.messages:
        css_class = "assistant-msg" if msg["role"] == "assistant" else "user-msg"
        avatar = "A" if msg["role"] == "assistant" else "U"
        with st.chat_message(msg["role"]):
            st.markdown(
                f"""
            <div class="{css_class}">
                <strong>{avatar}:</strong> {msg['content']}
                <div class="timestamp">{msg['time']}</div>
            </div>
            """,
                unsafe_allow_html=True,
            )


prompt = st.chat_input("Ask the Prolog-backed bot...")

if prompt:
    append_message("user", prompt.strip())
    payload = build_payload(prompt.strip(), model, temperature, max_tokens)
    reply, error = post_message(st.session_state.api_url, payload)

    if error:
        st.session_state.last_error = error
        append_message("assistant", f"Backend error: {error}")
    else:
        st.session_state.last_error = None
        append_message("assistant", reply or "No reply received.")

    st.rerun()

if st.session_state.last_error:
    st.error(st.session_state.last_error)
