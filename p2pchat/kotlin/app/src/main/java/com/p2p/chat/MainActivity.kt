package com.p2p.chat

import android.app.AlertDialog
import android.content.DialogInterface
import android.os.Build
import android.os.Bundle
import android.support.annotation.RequiresApi
import android.support.v7.app.AppCompatActivity
import android.util.Log
import android.view.View
import android.widget.EditText
import android.widget.ListView
import com.p2p.chat.R
import com.hypelabs.hype.*
import java.io.UnsupportedEncodingException

class MainActivity : AppCompatActivity(), StateObserver, NetworkObserver, MessageObserver {
    private var TAG = MainActivity::class.simpleName
    private var editText: EditText? = null
    private var messageAdapter: ChatMessageAdapter? = null
    private var messagesView: ListView? = null
    private var resolvedInstance: Instance? = null
    private var dialog: SingletonDialog = SingletonDialog()
    var RESOLVED_INSTANCE_TITLE : String = "Hype new instance resolved"
    var SEARCH_INSTANCE_TITLE : String = "Hype started..."
    var SEARCH_INSTANCE_BODY : String = "Searching for instances"
    var NO_INSTANCE_TITLE : String = "No resolved instance"
    var NO_INSTANCE_BODY : String = "Would you like to search for an instance?"
    var SENT_FAILED_TITLE : String = "Sending Failed"
    var SENT_TITLE : String = "Sending message..."
    var RECV_TITLE : String = "Delivered"

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        editText = findViewById<View>(R.id.editText) as EditText
        messageAdapter = ChatMessageAdapter(this)
        messagesView = findViewById<View>(R.id.messages_view) as ListView
        messagesView!!.adapter = messageAdapter
        requestHypeToStart()
    }

    protected fun requestHypeToStart() {
        Log.i(TAG, String.format("requestHypeToStart"))
        // The application context is used to query the user for permissions, such as using
        // the Bluetooth adapter or enabling Wi-Fi. The context must be set before anything
        // else is attempted, otherwise resulting in an exception being thrown.
        Hype.setContext(this@MainActivity)

        // Adding itself as an Hype state observer makes sure that the application gets
        // notifications for lifecycle events being triggered by the Hype SDK. These
        // events include starting and stopping, as well as some error handling.
        Hype.addStateObserver(this)

        // Need to implement this to announce to other user
        Hype.setAnnouncement("New user found".toByteArray())

        // Network observer notifications include other devices entering and leaving the
        // network. When a device is found all observers get a onHypeInstanceFound
        // notification, and when they leave onHypeInstanceLost is triggered instead.
        // This observer also gets notifications for onHypeInstanceResolved when an
        // instance is resolved.
        Hype.addNetworkObserver(this)

        // Message notifications indicate when messages are received, sent, or delivered.
        // Such callbacks are called with progress tracking indication.
        Hype.addMessageObserver(this)

        // App identifiers are used to segregate the network. Apps with different identifiers
        // do not communicate with each other, although they still cooperate on the network.
        Hype.setAppIdentifier("c990ae8f")

        // Requesting Hype to start is equivalent to requesting the device to publish
        // itself on the network and start browsing for other devices in proximity. If
        // everything goes well, the onHypeStart() observer method gets called, indicating
        // that the device is actively participating on the network.
        Hype.start()
    }

    override fun onHypeMessageReceived(message: com.hypelabs.hype.Message, instance: Instance) {
        Log.i(TAG, String.format("Hype message received %s %s", message.identifier, instance.stringIdentifier))
        var text: String? = null
        try {
            text = String(message.data, charset("UTF_8"))
            // If all goes well, this will log the original text
            Log.i(TAG, String.format("Hype received a message from: %s %s", instance.stringIdentifier, text))
        } catch (e: UnsupportedEncodingException) {
            e.printStackTrace()
        }
        val chatMessage = ChatMessage(message, MemberData("other", "red"), false)
        runOnUiThread {
            messageAdapter!!.add(chatMessage)
            messagesView!!.setSelection(messagesView!!.count - 1)
        }
    }

    override fun onHypeMessageFailedSending(messageInfo: MessageInfo, instance: Instance, error: Error) {
        Log.i(TAG, String.format("Hype message failed sending %s %s [%s]", messageInfo.identifier, instance.stringIdentifier, error.toString()))
        runOnUiThread {
            dialog.show(this@MainActivity, null, SENT_FAILED_TITLE,
                    String.format("Could not send to %s", instance.stringIdentifier))
        }
    }

    override fun onHypeMessageSent(messageInfo: MessageInfo, instance: Instance, v: Float, b: Boolean) {
        Log.i(TAG, String.format("Hype message sent %s %s [%f] %b", messageInfo.identifier, instance.stringIdentifier, v, b))
        runOnUiThread {
            dialog.show(this@MainActivity, null, SENT_TITLE, "")
        }
    }

    override fun onHypeMessageDelivered(messageInfo: MessageInfo, instance: Instance, v: Float, b: Boolean) {
        Log.i(TAG, String.format("Hype message delivered %s %s [%f] %b", messageInfo.identifier,
                instance.stringIdentifier, v, b))
        runOnUiThread {
            dialog.show(this@MainActivity, null, RECV_TITLE, "")
        }
    }

    fun shouldResolveInstance(instance: Instance?): Boolean {
        // This method should decide whether an instance is interesting for communicating.
        // For that purpose, the implementation could use instance.userIdentifier, but it's
        // noticeable that announcements may not be available yet. Announcements are only
        // exchanged during the handshake.
        return true
    }

    override fun onHypeInstanceFound(instance: Instance) {
        Log.i(TAG, "found instance: " + instance.appStringIdentifier)
        // Instances need to be resolved before being ready for communicating. This will
        // force the two of them to perform an handshake.
        if (shouldResolveInstance(instance)) {
            Hype.resolve(instance)
        }
    }

    @RequiresApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
    override fun onHypeInstanceLost(instance: Instance, error: Error) {
        Log.i(TAG, String.format("Hype lost instance: %s [%s]", instance.stringIdentifier,
                error.toString()))
        // This instance is no longer available for communicating. If the instance
        // is somehow being tracked, such as by a map of instances, this would be
        // the proper time for cleanup.
        runOnUiThread {
            dialog.show(this@MainActivity, null,
                    SEARCH_INSTANCE_TITLE, SEARCH_INSTANCE_BODY)
        }
    }

    override fun onHypeInstanceResolved(instance: Instance) {
        Log.i(TAG, String.format("Hype resolved instance: %s", instance.stringIdentifier))
        // At this point the instance is ready to communicate. Sending and receiving
        // content is possible at any time now.
        var listener : DialogInterface.OnClickListener = DialogInterface.OnClickListener {
            _, _ ->
            Log.d(TAG, "Hype will communicate with instance")
            this.resolvedInstance = instance
        }
        runOnUiThread {
            dialog.show(this@MainActivity, listener, RESOLVED_INSTANCE_TITLE,
                    String.format("Instance found: %s\nDo you wish to communicate?", instance.stringIdentifier))
        }
    }

    override fun onHypeInstanceFailResolving(instance: Instance, error: Error) {
        Log.i(TAG, String.format("Hype could not resolve instance: %s [%s]", instance.stringIdentifier, error.toString()))
    }

    @RequiresApi(Build.VERSION_CODES.JELLY_BEAN_MR1)
    override fun onHypeStart() {
        Log.i(TAG, "Hype started")
        runOnUiThread {
            dialog.show(this@MainActivity, null,
                    SEARCH_INSTANCE_TITLE, SEARCH_INSTANCE_BODY)
        }
    }

    override fun onHypeStop(error: Error) {
        error.let{
            Log.i(TAG, String.format("Hype stopped [%s]", error.toString()))
        }
        Log.i(TAG, "Hype stopped [null]. You should do reinstall the application.")
    }

    override fun onHypeFailedStarting(error: Error) {
        Log.i(TAG, String.format("Hype failed starting [%s]", error.toString()))
    }

    override fun onHypeReady() {
        Log.i(TAG, String.format("Hype is ready"))
    }

    override fun onHypeStateChange() {
        when (Hype.getState()) {
            State.Starting -> Log.i(TAG, "Hype is in starting state")
            State.Idle -> Log.i(TAG, "Hype is in idle state")
            State.Running -> Log.i(TAG, "Hype is in running state")
            State.Stopping -> Log.i(TAG, "Hype is in stopping state")
        }
    }

    override fun onHypeRequestAccessToken(userIdentifier: Int): String {
        return "3905669394fa2533"
    }

    @Throws(UnsupportedEncodingException::class)
    protected fun sendMessage(text: String, instance: Instance?, acknowledge: Boolean): com.hypelabs.hype.Message {

        // When sending content there must be some sort of protocol that both parties
        // understand. In this case, we simply send the text encoded in UTF-8. The data
        // must be decoded when received, using the same encoding.
        val data = text.toByteArray(charset("UTF-8"))
        return Hype.send(data, instance, acknowledge)
    }

    fun sendMessage(view: View?) {
        println("sendMessage")
        val message = editText!!.text.toString()
        if (message.length > 0) {
            val sentMessage: com.hypelabs.hype.Message
            try {
                sentMessage = sendMessage(message, resolvedInstance, true)
                val mData = MemberData("Bobby", "")
                val m = ChatMessage(sentMessage, mData, true)
                runOnUiThread {
                    messageAdapter!!.add(m)
                    messagesView!!.setSelection(messagesView!!.count - 1)
                }
                editText!!.text.clear()
            } catch (e: UnsupportedEncodingException) {
                e.printStackTrace()
            }
            catch (e: Exception) {
                var listener : DialogInterface.OnClickListener = DialogInterface.OnClickListener {
                    _, _ ->
                    Log.i(TAG, "Hype no instance yes clicked")
                    requestHypeToStart()
                }
                runOnUiThread {
                    dialog.show(this@MainActivity, listener, NO_INSTANCE_TITLE,
                            NO_INSTANCE_BODY)
                }
            }
        }
    }

}

class MemberData {
    var name: String? = null
        private set
    var color: String? = null
        private set

    constructor(name: String?, color: String?) {
        this.name = name
        this.color = color
    }

    override fun toString(): String {
        return "MemberData{" +
                "name='" + name + '\'' +
                ", color='" + color + '\'' +
                '}'
    }
}