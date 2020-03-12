package com.example.scaledrone.chat;

public class ChatMessage {
    private String text;
    private MemberData memberData;
    private boolean belongsToCurrentUser;

    public ChatMessage(String text, MemberData data, boolean belongsToCurrentUser) {
        this.text = text;
        this.memberData = data;
        this.belongsToCurrentUser = belongsToCurrentUser;
    }

    public String getText() {
        return text;
    }

    public MemberData getMemberData() {
        return memberData;
    }

    public boolean isBelongsToCurrentUser() {
        return belongsToCurrentUser;
    }
}
