import streamlit as st
import requests
from datetime import datetime

# ===================== PAGE CONFIG =====================
st.set_page_config(
    page_title="AI Chat",
    page_icon="🤖",
    layout="centered",  # Better for classic chat look
    initial_sidebar_state="expanded"
)

# ===================== CUSTOM CSS =====================
st.markdown("""
<style>
    .stApp {
        background: linear-gradient(135deg, #0a0e27, #1a1a2e, #16213e);
        color: #e0e0e0;
    }
    #MainMenu, footer, .stDeployButton {visibility: hidden;}
    
    h1 {
        text-align: center;
        font-size: 2.8rem;
        background: linear-gradient(90deg, #00d2ff, #3a7bd5, #8e44ad);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        margin-bottom: 0.5rem;
    }
    .subtitle {
        text-align: center;
        color: #aaa;
        margin-bottom: 2rem;
    }
    
    .chat-container {
        max-width: 800px;
        margin: 0 auto;
        padding: 20px;
        min-height: 60vh;
        max-height: 60vh;
        overflow-y: auto;
        display: flex;
        flex-direction: column;
        gap: 15px;
    }
    
    .message {
        display: flex;
        align-items: flex-start;
        max-width: 80%;
        animation: fadeIn 0.4s ease-out;
    }
    .message.user {
        align-self: flex-end;
        flex-direction: row-reverse;
    }
    .message.user .bubble {
        background: linear-gradient(135deg, #667eea, #764ba2);
        border-radius: 18px 18px 4px 18px;
    }
    .message.ai .bubble {
        background: rgba(60, 60, 90, 0.9);
        border-radius: 18px 18px 18px 4px;
    }
    
    .bubble {
        padding: 14px 20px;
        border-radius: 18px;
        max-width: 100%;
        word-wrap: break-word;
        box-shadow: 0 2px 8px rgba(0,0,0,0.3);
    }
    
    .avatar {
        width: 42px;
        height: 42px;
        border-radius: 50%;
        margin: 0 12px;
        flex-shrink: 0;
    }
    
    .typing-indicator .bubble {
        background: rgba(60, 60, 90, 0.9);
        padding: 14px 20px;
        display: inline-block;
    }
    .typing-indicator span {
        height: 8px;
        width: 8px;
        background: #aaa;
        border-radius: 50%;
        display: inline-block;
        margin: 0 3px;
        animation: bounce 1.4s infinite;
    }
    .typing-indicator span:nth-child(2) { animation-delay: 0.2s; }
    .typing-indicator span:nth-child(3) { animation-delay: 0.4s; }
    
    @keyframes bounce {
        0%, 80%, 100% { transform: translateY(0); }
        40% { transform: translateY(-10px); }
    }
    @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
    }
    
    .input-area {
        max-width: 800px;
        margin: 20px auto;
        padding: 0 20px;
    }
</style>
""", unsafe_allow_html=True)

# ===================== SESSION STATE =====================
if "messages" not in st.session_state:
    st.session_state.messages = []
if "waiting_for_response" not in st.session_state:
    st.session_state.waiting_for_response = False

# ===================== SIDEBAR SETTINGS =====================
with st.sidebar:
    st.markdown("## ⚙️ Settings")
    model = st.selectbox("Model", ["gpt-3.5-turbo", "gpt-4", "gpt-4-turbo"], index=0)
    temperature = st.slider("Temperature", 0.0, 1.0, 0.7, 0.05)
    max_tokens = st.slider("Max Tokens", 100, 4000, 1000, 50)
    
    if st.button("🗑️ Clear Chat History"):
        st.session_state.messages = []
        st.session_state.waiting_for_response = False
        st.rerun()

# ===================== MAIN UI =====================
st.markdown("<h1>🤖 AI Chat</h1>", unsafe_allow_html=True)
st.markdown("<p class='subtitle'>Simple • Modern • Intelligent</p>", unsafe_allow_html=True)

# Chat container
st.markdown("<div class='chat-container'>", unsafe_allow_html=True)

# Display messages
for msg in st.session_state.messages:
    role = msg["role"]
    content = msg["content"]
    time = msg["time"]

    if role == "user":
        st.markdown(f"""
        <div class="message user">
            <img class="avatar" src="https://api.dicebear.com/7.x/avataaars/svg?seed=user">
            <div class="bubble">{content}<div style="font-size:0.8em;color:#ccc;margin-top:8px;">{time}</div></div>
        </div>
        """, unsafe_allow_html=True)
    else:
        st.markdown(f"""
        <div class="message ai">
            <img class="avatar" src="https://api.dicebear.com/7.x/bottts/svg?seed=ai">
            <div class="bubble">{content}<div style="font-size:0.8em;color:#ccc;margin-top:8px;">{time}</div></div>
        </div>
        """, unsafe_allow_html=True)

# Show typing indicator if waiting
if st.session_state.waiting_for_response:
    st.markdown("""
    <div class="message ai">
        <img class="avatar" src="https://api.dicebear.com/7.x/bottts/svg?seed=ai">
        <div class="bubble typing-indicator">
            <span></span><span></span><span></span>
        </div>
    </div>
    """, unsafe_allow_html=True)

st.markdown("</div>", unsafe_allow_html=True)

# ===================== INPUT AREA =====================
st.markdown("<div class='input-area'>", unsafe_allow_html=True)
cols = st.columns([6, 1])
with cols[0]:
    user_input = st.text_input("", placeholder="Type your message here...", key="user_input", label_visibility="collapsed")
with cols[1]:
    send_button = st.button("Send", use_container_width=True)

st.markdown("</div>", unsafe_allow_html=True)

# ===================== HANDLE SEND =====================
API_URL = "http://localhost:8000/chat"  # Change if your backend runs elsewhere

if send_button and user_input.strip() and not st.session_state.waiting_for_response:
    user_message = user_input.strip()
    current_time = datetime.now().strftime("%H:%M")

    # Append user message
    st.session_state.messages.append({
        "role": "user",
        "content": user_message,
        "time": current_time
    })

    # Set waiting state
    st.session_state.waiting_for_response = True
    st.rerun()

# ===================== CALL BACKEND WHEN WAITING =====================
if st.session_state.waiting_for_response:
    try:
        payload = {
            "message": st.session_state.messages[-1]["content"],  # Only last message
            "model": model,
            "temperature": temperature,
            "max_tokens": max_tokens
            # If your backend supports history, send full messages instead
        }
        with st.spinner(""):
            response = requests.post(API_URL, json=payload, timeout=60)
        
        if response.status_code == 200:
            ai_reply = response.json().get("reply", "No response from AI.")
        else:
            ai_reply = f"⚠️ Error {response.status_code}: {response.text}"
    except requests.exceptions.RequestException as e:
        ai_reply = f"⚠️ Connection error: {str(e)}"

    current_time = datetime.now().strftime("%H:%M")
    st.session_state.messages.append({
        "role": "assistant",
        "content": ai_reply,
        "time": current_time
    })
    st.session_state.waiting_for_response = False
    st.rerun()