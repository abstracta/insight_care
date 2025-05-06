from abc import ABC, abstractmethod
from typing import List, Tuple, Dict, Type
import openai
import os
from insight_care.insight_care_prompts import Prompts, Messages, Continue


class State(ABC):
    _inquire_amount = 5
    _re_question_amount = 5

    def __init__(
        self,
        next_state: Type["State"],
        previous_state: Type["State"],
        message: str,
    ) -> None:
        self.next_state = next_state
        self.previous_state = previous_state
        self.message = message

    @abstractmethod
    def on_message(self, session: Dict) -> Tuple["State", Dict]:
        pass

    @abstractmethod
    def get_message(self, session: Dict) -> str:
        pass

    def get_ranking(self, session: Dict) -> int:
        ranking = self.get_completion(session, Prompts.WHO_ANALYSIS_PROMPT.value)
        return int(ranking)

    def is_affirmative_answer(self, session: Dict, question: str) -> str:
        return self.get_completion(
            session, Prompts.SHOULD_CONTINUE_PROMPT.value.format(question=question)
        )

    def get_message_variation(self, session: Dict, message: str) -> str:
        return self.get_completion(
            session, Prompts.MESSAGE_VARIATION_PROMPT.value.format(msg=message)
        )

    def get_completion(self, session: Dict, message: str) -> str:
        messages = session["messages"].copy()
        messages.append(
            {
                "role": "system",
                "content": message,
            }
        )
        client = openai.AzureOpenAI(
            base_url=os.getenv("OPENAI_BASE_URL"), api_key=os.getenv("OPENAI_API_KEY")
        )
        response = client.chat.completions.create(
            model=os.getenv("LLM_MODEL"),
            temperature=float(os.getenv("LLM_TEMPERATURE")),
            messages=messages,
        )
        return response.choices[0].message.content


class ConfirmationStates(State):
    def __init__(
        self,
        next_state: Type[State],
        previous_state: Type[State],
        message: str,
    ) -> None:
        super().__init__(next_state, previous_state, message)
        self.counter = 0
        self.continue_ = Continue.FALSE.value
        self.question = ""

    def on_message(self, session: Dict) -> Tuple["State", Dict]:
        self.continue_ = self.is_affirmative_answer(session, self.question)
        if self.continue_ == Continue.TRUE.value:
            return self.next_state(), session
        elif (
            self.counter < self._re_question_amount
            and self.continue_ == Continue.UNDETERMINED.value
        ):
            return self, session
        else:
            return TerminateState(), session

    def get_message(self, session: Dict) -> str:
        if self.continue_ == Continue.UNDETERMINED.value:
            self.counter += 1
            return self.get_completion(
                session,
                Prompts.CONFIRMATION_INQUIRE_CONTINUE_PROMPT.value.format(
                    question=self.message
                ),
            )
        else:
            self.question = self.get_message_variation(session, self.message)
            return self.question


class WhoStates(State):
    def __init__(
        self,
        next_state: Type[State],
        previous_state: Type[State],
        message: str,
    ) -> None:
        super().__init__(next_state, previous_state, message)
        self.counter = 0
        self.ranking = 0
        self.continue_ = Continue.FALSE.value

    def on_message(self, session: Dict) -> Tuple["State", Dict]:
        ranking = self.get_ranking(session)
        if ranking == 0 and self.counter < self._re_question_amount:
            self.counter += 1
            self.continue_ = Continue.UNDETERMINED.value
            return self, session
        elif ranking != 0:
            session["ranking"][self.name] = ranking
            session["ranking"]["WHO_2"] = 5
            session["ranking"]["WHO_3"] = 5
            session["ranking"]["WHO_4"] = 5
            session["ranking"]["WHO_5"] = 5
            return self.next_state(), session

    def get_message(self, session: Dict) -> str:
        if self.continue_ == Continue.UNDETERMINED.value:
            return self.get_completion(session, Prompts.WHO_RE_QUESTION_PROMPT.value)
        else:
            return self.get_completion(
                session, Prompts.WHO_INQUIRE_CLOSE.value.format(question=self.message)
            )


class WhoInquire(State):
    def __init__(
        self,
        next_state: Type[State],
        previous_state: Type["State"],
        message: str,
    ) -> None:
        super().__init__(next_state, previous_state, message)
        self.counter = 0
        self.inquire = True

    def on_message(self, session: Dict) -> Tuple["State", Dict]:
        ranking = session["ranking"][self.previous_state.name]
        question = self.message
        questions = Messages.QUESTIONS.value[self.previous_state.name][ranking]
        self.inquire = self.should_inquire_who(session, question, questions)
        if self.inquire == Continue.TRUE.value and self.counter < self._inquire_amount:
            self.counter += 1
            return self, session
        else:
            return self.next_state(), session

    def get_message(self, session: Dict) -> str:
        ranking = session["ranking"][self.previous_state.name]
        question = self.message
        questions = Messages.QUESTIONS.value[self.previous_state.name][ranking]
        return self.get_completion(
            session,
            Prompts.WHO_INQUIRE.value.format(questions=questions, question=question),
        )

    def should_inquire_who(
        self, session: Dict, question: str, questions: List[str]
    ) -> str:
        return self.get_completion(
            session,
            Prompts.WHO_SHOULD_INQUIRE.value.format(
                question=question, questions=questions
            ),
        )


class IntroState(ConfirmationStates):

    name = "INTRO"

    def __init__(self) -> None:
        super().__init__(AgreementState, None, Messages.INITIAL_MESSAGE.value)


class AgreementState(ConfirmationStates):

    name = "AGREEMENT"

    def __init__(self) -> None:
        super().__init__(ExplanationState, IntroState, Messages.AGREEMENT_MESSAGE.value)


class ExplanationState(ConfirmationStates):

    name = "EXPLANATION"

    def __init__(self) -> None:
        super().__init__(Who1, AgreementState, Messages.EXPLANATION_MESSAGE.value)


class Who1(WhoStates):

    name = "WHO_1"

    def __init__(self) -> None:
        super().__init__(Who1Inquire, ExplanationState, Messages.WHO_1_MESSAGE.value)


class Who1Inquire(WhoInquire):

    name = "WHO_1_INQUIRE"

    def __init__(self) -> None:
        super().__init__(Who2, Who1, Messages.WHO_1_MESSAGE.value)


class Who2(WhoStates):

    name = "WHO_2"

    def __init__(self) -> None:
        super().__init__(Who2Inquire, Who1Inquire, Messages.WHO_2_MESSAGE.value)


class Who2Inquire(WhoInquire):

    name = "WHO_2_INQUIRE"

    def __init__(self) -> None:
        super().__init__(Who3, Who2, Messages.WHO_2_MESSAGE.value)


class Who3(WhoStates):

    name = "WHO_3"

    def __init__(self) -> None:
        super().__init__(Who3Inquire, Who2Inquire, Messages.WHO_3_MESSAGE.value)


class Who3Inquire(WhoInquire):

    name = "WHO_3_INQUIRE"

    def __init__(self) -> None:
        super().__init__(Who4, Who3, Messages.WHO_3_MESSAGE.value)


class Who4(WhoStates):

    name = "WHO_4"

    def __init__(self) -> None:
        super().__init__(Who4Inquire, Who3Inquire, Messages.WHO_4_MESSAGE.value)


class Who4Inquire(WhoInquire):

    name = "WHO_4_INQUIRE"

    def __init__(self) -> None:
        super().__init__(Who5, Who4, Messages.WHO_4_MESSAGE.value)


class Who5(WhoStates):

    name = "WHO_5"

    def __init__(self) -> None:
        super().__init__(Who5Inquire, Who4Inquire, Messages.WHO_5_MESSAGE.value)


class Who5Inquire(WhoInquire):

    name = "WHO_5_INQUIRE"

    def __init__(self) -> None:
        super().__init__(Assessment, Who5, Messages.WHO_5_MESSAGE.value)


class Assessment(State):

    name = "ASSESSMENT"

    def __init__(self) -> None:
        super().__init__(Report, Who5Inquire, Prompts.ASSESSMENT_PROMPT.value)
        self.keep_talking = Continue.FALSE.value
        self.question = ""

    def on_message(self, session: Dict) -> Tuple["State", Dict]:
        self.keep_talking = self.get_completion(session, Prompts.SHOULD_KEEP_TALKING.value.format(question=self.question))
        if self.keep_talking == Continue.TRUE.value:
            return self, session
        else:
            return self.next_state(), session

    def get_message(self, session: Dict) -> str:
        if self.keep_talking == Continue.TRUE.value:
            return self.get_completion(
                session,
                Prompts.KEEP_TALKING_PROMPT.value.format(question=self.question),
            )
        else:
            score = sum((6 - value) * 4 for _, value in session["ranking"].items())
            example = next(
                (
                    item["example"]
                    for item in Messages.ASSESSMENTS_MESSAGES.value
                    if item["score_start"] <= score <= item["score_end"]
                ),
                None,
            )
            self.question = self.get_completion(
                session, self.message.format(score=round(score), example=example)
            )
            return self.question


class Report(State):

    name = "REPORT"

    def __init__(self) -> None:
        super().__init__(Close, Assessment, Prompts.FINAL_REPORT_PROMPT.value)
        self.continue_ = Continue.FALSE.value
        self.question = ""

    def on_message(self, session: Dict) -> Tuple["State", Dict]:
        self.continue_ = self.is_affirmative_answer(session, self.question)
        if self.continue_ == Continue.TRUE.value or self.continue_ == Continue.FALSE.value:
            return self.next_state(), session
        else:
            return self, session

    def get_message(self, session: Dict) -> str:
        if self.continue_ == Continue.UNDETERMINED.value:
            return self.get_completion(
                session,
                Prompts.KEEP_TALKING_PROMPT.value.format(question=self.question),
            )
        else:
            self.question = self.get_completion(session, self.message)
            return self.question


class Close(State):

    name = "CLOSE"

    def __init__(self) -> None:
        super().__init__(IntroState, Report, Prompts.CLOSER_PROMPT.value)
        self.keep_talking = Continue.FALSE.value

    def on_message(self, session: Dict) -> Tuple["State", Dict]:
        state = IntroState()
        return self.next_state(), {"state": state, "messages": [], "ranking": []}

    def get_message(self, session: Dict) -> str:
        return self.get_completion(session, self.message)


class TerminateState(State):

    name = "TERMINATE"

    def __init__(self) -> None:
        super().__init__(IntroState, None, Prompts.TERMINATE_PROMPT.value)

    def on_message(
        self, messages: List[Dict[str, str]], session: Dict
    ) -> Tuple["State", Dict]:
        state = IntroState()
        return state, {"state": state, "messages": [], "ranking": []}

    def get_message(self, session: Dict) -> str:
        return self.get_completion(session, self.message)
