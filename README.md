
# AI Chatbot in Prolog 

A simple logic-based chatbot where **Prolog serves the AI logic** via an API, and **Python with Streamlit provides the user interface**.  
This project demonstrates combining logic programming with a modern interactive interface.

##  Overview

- **Prolog AI logic**: Handles reasoning, pattern matching, and conversational rules.  
- **Python / Streamlit**: Provides a user-friendly interface and communicates with Prolog via API.  
- Ideal for learning **logic programming**, API integration, and simple conversational AI.

###  Key Concepts

- Prolog for symbolic reasoning and rule-based conversation
- Python Streamlit interface for interactive chat
- Prolog and Python communicate via a lightweight API


| File | Role |
|------|------|
| `server.pl` | Prolog chatbot core and API logic |
| `app.py` | Python Streamlit interface for interacting with Prolog |
| `Data.json` | Phrase templates, responses and dialog rules |
| `LICENSE` | MIT open source license |

##  Features

✔ Prolog AI logic via API  
✔ Interactive Python Streamlit interface  
✔ Rule-based responses, easily extendable  
✔ Lightweight, modular design

##  Getting Started

### Requirements

- SWI-Prolog (or another Prolog implementation)  
- Python 3.x  
- Streamlit (`pip install streamlit`)  

### Install

1. Clone the repo:

   `bash
   git clone https://github.com/etrnkz/ai-chatbot-prolog.git
   cd ai-chatbot-prolog
   `

2. Install Python dependencies:

   `bash
   pip install -r requirements.txt
   `

3. Install SWI-Prolog:

   Follow instructions at https://www.swi-prolog.org/

### Run the Chatbot

1. **Start the Prolog API server:**

   `bash
   swipl server.pl
   `

2. **Start the Streamlit interface:**

   `bash
   streamlit run web.py
   `

3. Open your browser at the URL shown by Streamlit and start chatting.

##  Example

Sample conversation:

You: Hello!
Bot: Hi there! How can I help you today?


##  License

This project is licensed under the **MIT License** — see the [LICENSE](LICENSE) file for details.

##  Contributing

Contributions, improvements, and suggestions are welcome! Open issues or submit pull requests.
