---
description: Interactively create a prd for LLM execution
allowed-tools: Bash(git add:*), Bash(git status:*), Bash(git commit:*)
---
# Rule: Generating a Product Requirements Document (PRD)

## Goal of this command

To guide an AI assistant in creating a detailed Product Requirements Document (PRD) in Markdown format, based on an initial user prompt. The PRD should be clear, actionable, and suitable for an LLM (at the experience level of a junior developer) to understand and implement the feature.

## Core Directives

1. Do NOT start implementing the PRD
2. Make sure to ask the user clarifying questions
3. Take the user's answers to the clarifying questions and improve the PRD

## Process

1.  **Receive Initial Prompt:** The user provides a brief description or request for a new feature or functionality.
2.  **Clarify and Confirm:** Before writing the PRD, the AI must ensure it has answers to the key clarifying questions. It should first analyze the user's prompt to see if any answers can be inferred. If so, it must **confirm these inferences** with the user (e.g., "It sounds like the goal is X, is that correct?"). For any remaining unanswered questions, it should ask the user directly. The target outcome of the exchange is to understand the "what" and "why" of the feature, not necessarily the "how" (which the developer will figure out). Make sure to provide options in letter/number lists so I can respond easily with my selections. The AI should ensure that best practices are followed. Don't let the user get away with specifying things vaguely enough that things would be unclear to a model that doesn't have strong reasoning capabilities. Prefer framing features in a way that translate to clear implementation. 
3.  **Generate PRD:** Based on the initial prompt and the user's answers to the clarifying questions, generate a PRD using the structure outlined below.
4.  **Confirm and Generate:** Summarize your understanding for the user and ask for confirmation to proceed (e.g., "I will now create a PRD for a feature that does X for Y users. Shall I proceed?").
5.  **Create Directory and Save PRD:** Once confirmed, generate a file-safe `[feature-name]` from the prompt. Create the directory `/specs/[feature-name]/` if it doesn't already exist. Finally, save the PRD content to `/specs/[feature-name]/prd-[feature-name].md`.

## Clarifying Questions (Examples)

The AI should adapt its questions based on the prompt, but here are some common areas to explore:

*   **Problem/Goal:** "What problem does this feature solve for the user?" or "What is the main goal we want to achieve with this feature?"
*   **Target User:** "Who is the primary user of this feature?"
*   **Core Functionality:** "Can you describe the key actions a user should be able to perform with this feature?"
*   **User Stories:** "Could you provide a few user stories? (e.g., As a [type of user], I want to [perform an action] so that [benefit].)"
*   **Acceptance Criteria:** "How will we know when this feature is successfully implemented? What are the key success criteria?"
*   **Scope/Boundaries:** "Are there any specific things this feature *should not* do (non-goals)?"
*   **Data Requirements:** "What kind of data does this feature need to display or manipulate?"
*   **Design/UI:** "Are there any design preferences, mockups, or existing UI components I should be aware of?" or "Can you describe the desired look and feel?"
*   **Technical Constraints:** "Are there any technical constraints, dependencies, or specific libraries I should know about for this feature?"
*   **Edge Cases:** "Are there any potential edge cases or error conditions we should consider?"

## PRD Structure

The generated PRD should include the following sections:

1.  **Introduction/Overview:** Briefly describe the feature and the problem it solves. State the goal.
2.  **Goals:** List the specific, measurable objectives for this feature.
3.  **Assumptions:** List any business, technical, or user assumptions being made (e.g., "Users are logged in," "Data is in English").
4.  **User Stories:** Detail the user narratives describing feature usage and benefits.
5.  **Functional Requirements:** List the specific functionalities the feature must have. Use clear, concise language (e.g., "The system must allow users to upload a profile picture."). Number these requirements.
6.  **Non-Goals (Out of Scope):** Clearly state what this feature will *not* include to manage scope.
7.  **Design Considerations (Optional):** Link to mockups, describe UI/UX requirements, or mention relevant components/styles if applicable.
8.  **Technical Considerations (Optional):** Mention any known technical constraints, dependencies, or suggestions (e.g., "Should integrate with the existing Auth module").
9.  **Success Metrics:** How will the success of this feature be measured? (e.g., "Increase user engagement by 10%", "Reduce support tickets related to X").
10. **Open Questions:** List any remaining questions or areas needing further clarification.

## Target Audience

Assume the primary reader of the PRD is an LLM with the capability of a  **junior developer**. Therefore, requirements should be explicit, unambiguous, and avoid jargon where possible. Provide enough detail for them to understand the feature's purpose and core logic.

## Output

*   **Format:** Markdown (`.md`)
*   **Location:** `/specs/[feature-name]/`
*   **Filename:** `prd-[feature-name].md`
*   **Note:** The `[feature-name]` is a short, file-safe string generated from the user's initial prompt.
