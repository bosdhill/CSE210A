package com.example.scaledrone.chat;

import android.app.Activity;
import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.GradientDrawable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;
//import com.hypelabs.hype.Message;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;


public class ChatMessageAdapter extends BaseAdapter {

    List<ChatMessage> messages = new ArrayList<ChatMessage>();
    Context context;

    public ChatMessageAdapter(Context context) {
        this.context = context;
    }


    public void add(ChatMessage message) {
        this.messages.add(message);
        notifyDataSetChanged();
    }

    @Override
    public int getCount() {
        return messages.size();
    }

    @Override
    public Object getItem(int i) {
        return messages.get(i);
    }

    @Override
    public long getItemId(int i) {
        return i;
    }

    @Override
    public View getView(int i, View convertView, ViewGroup viewGroup) {
        MessageViewHolder holder = new MessageViewHolder();
        LayoutInflater messageInflater = (LayoutInflater) context.getSystemService(Activity.LAYOUT_INFLATER_SERVICE);
        ChatMessage message = messages.get(i);

        if (message.isBelongsToCurrentUser()) {
            convertView = messageInflater.inflate(R.layout.my_message, null);
            holder.messageBody = (TextView) convertView.findViewById(R.id.message_body);
            convertView.setTag(holder);
            try {
                String text = new String(message.getMessage().getData(), "UTF-8");
                holder.messageBody.setText(text);
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
        } else {
            convertView = messageInflater.inflate(R.layout.their_message, null);
            holder.avatar = (View) convertView.findViewById(R.id.avatar);
            holder.name = (TextView) convertView.findViewById(R.id.name);
            holder.messageBody = (TextView) convertView.findViewById(R.id.message_body);
            convertView.setTag(holder);

            holder.name.setText(message.getMemberData().getName());
            try {
                String text = new String(message.getMessage().getData(), "UTF-8");
                holder.messageBody.setText(text);
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
            GradientDrawable drawable = (GradientDrawable) holder.avatar.getBackground();
            drawable.setColor(Color.parseColor(message.getMemberData().getColor()));
        }

        return convertView;
    }

}

class MessageViewHolder {
    public View avatar;
    public TextView name;
    public TextView messageBody;
}