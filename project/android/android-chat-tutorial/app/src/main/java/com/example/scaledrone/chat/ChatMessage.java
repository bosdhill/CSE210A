package com.example.scaledrone.chat;
import com.hypelabs.hype.*;

public class ChatMessage {
    private Message message;
    private MemberData memberData;
    private boolean belongsToCurrentUser;

    public ChatMessage(Message message, MemberData data, boolean belongsToCurrentUser) {
        this.message = message;
        this.memberData = data;
        this.belongsToCurrentUser = belongsToCurrentUser;
    }

    public Message getMessage() {
        return message;
    }

    public MemberData getMemberData() {
        return memberData;
    }

    public boolean isBelongsToCurrentUser() {
        return belongsToCurrentUser;
    }
}
