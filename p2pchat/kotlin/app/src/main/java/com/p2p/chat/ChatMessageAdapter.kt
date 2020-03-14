package com.p2p.chat

import android.app.Activity
import android.content.Context
import android.graphics.Color
import android.graphics.drawable.GradientDrawable
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.BaseAdapter
import android.widget.TextView
import com.p2p.chat.R
import java.io.UnsupportedEncodingException
import java.util.*

class ChatMessageAdapter(var context: Context) : BaseAdapter() {
    var messages: MutableList<ChatMessage> = ArrayList()
    fun add(message: ChatMessage) {
        messages.add(message)
        notifyDataSetChanged()
    }

    override fun getCount(): Int {
        return messages.size
    }

    override fun getItem(i: Int): Any {
        return messages[i]
    }

    override fun getItemId(i: Int): Long {
        return i.toLong()
    }

    override fun getView(i: Int, convertView: View?, viewGroup: ViewGroup): View {
        var convertView = convertView
        val holder = MessageViewHolder()
        val messageInflater = context.getSystemService(Activity.LAYOUT_INFLATER_SERVICE) as LayoutInflater
        val message = messages[i]
        if (message.isBelongsToCurrentUser) {
            convertView = messageInflater.inflate(R.layout.my_message, null)
            holder.messageBody = convertView.findViewById<View>(R.id.message_body) as TextView
            convertView.tag = holder
            try {
                val text = String(message.message.data, charset("UTF-8"))
                holder.messageBody!!.text = text
            } catch (e: UnsupportedEncodingException) {
                e.printStackTrace()
            }
        } else {
            convertView = messageInflater.inflate(R.layout.their_message, null)
            holder.avatar = convertView.findViewById(R.id.avatar) as View
            holder.name = convertView.findViewById<View>(R.id.name) as TextView
            holder.messageBody = convertView.findViewById<View>(R.id.message_body) as TextView
            convertView.tag = holder
            holder.name!!.text = message.memberData.name
            try {
                val text = String(message.message.data, charset("UTF-8"))
                holder.messageBody!!.text = text
            } catch (e: UnsupportedEncodingException) {
                e.printStackTrace()
            }
            val drawable = holder.avatar!!.background as GradientDrawable
            drawable.setColor(Color.parseColor(message.memberData.color))
        }
        return convertView
    }

}

internal class MessageViewHolder {
    var avatar: View? = null
    var name: TextView? = null
    var messageBody: TextView? = null
}