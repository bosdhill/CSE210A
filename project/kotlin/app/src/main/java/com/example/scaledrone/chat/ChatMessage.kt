package com.example.scaledrone.chat

import com.hypelabs.hype.Message

class ChatMessage(val message: Message, val memberData: MemberData, val isBelongsToCurrentUser: Boolean)