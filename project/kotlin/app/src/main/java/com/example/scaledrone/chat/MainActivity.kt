package com.example.scaledrone.chat

import android.app.AlertDialog
import android.os.Build
import android.os.Bundle
import android.support.annotation.RequiresApi
import android.support.v7.app.AppCompatActivity
import android.util.Log
import android.view.View
import android.widget.EditText
import android.widget.ListView
import com.example.scaledrone.chat.MainActivity
import com.fasterxml.jackson.core.JsonProcessingException
import com.fasterxml.jackson.databind.ObjectMapper
import com.hypelabs.hype.*
import com.scaledrone.lib.*
import com.scaledrone.lib.Message
import java.io.UnsupportedEncodingException
import java.util.*
import java.nio.charset.StandardCharsets;

class MainActivity : AppCompatActivity(), RoomListener, StateObserver, NetworkObserver, MessageObserver {
    private val channelID = "CHANNEL_ID_FROM_YOUR_SCALEDRONE_DASHBOARD"
    private val roomName = "observable-room"
    private var editText: EditText? = null
    private var scaledrone: Scaledrone? = null
    private val communicator: Communicator? = null
    private var messageAdapter: ChatMessageAdapter? = null
    private var messagesView: ListView? = null
    private var resolvedInstance: Instance? = null
    var isAttached = false
    var isResolveDialogOpen = false
        private set
    private var dialog: AlertDialog? = null
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        editText = findViewById<View>(R.id.editText) as EditText
        messageAdapter = ChatMessageAdapter(this)
        messagesView = findViewById<View>(R.id.messages_view) as ListView
        messagesView!!.adapter = messageAdapter
        val data = MemberData(randomName, randomColor)
        requestHypeToStart()
        scaledrone = Scaledrone(channelID, data)
        scaledrone!!.connect(object : Listener {
            override fun onOpen() {
                println("Scaledrone connection open")
                scaledrone!!.subscribe(roomName, this@MainActivity)
            }

            override fun onOpenFailure(ex: Exception) {
                System.err.println(ex)
            }

            override fun onFailure(ex: Exception) {
                System.err.println(ex)
            }

            override fun onClosed(reason: String) {
                System.err.println(reason)
            }
        })
    }

    protected fun requestHypeToStart() {
        Log.i(TAG, String.format("requestHypeToStart"))
        // The application context is used to query the user for permissions, such as using
        // the Bluetooth adapter or enabling Wi-Fi. The context must be set before anything
        // else is attempted, otherwise resulting in an exception being thrown.
        Hype.setContext(applicationContext)

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

    @RequiresApi(Build.VERSION_CODES.KITKAT)
    override fun onHypeMessageReceived(message: com.hypelabs.hype.Message, instance: Instance) {
        Log.i(TAG, String.format("Hype message received %s %s", message.identifier, instance.stringIdentifier))
        var text: String? = null
        try {
            text = String(message.data, StandardCharsets.UTF_8)
            // If all goes well, this will log the original text
            Log.i(TAG, String.format("Hype received a message from: %s %s", instance.stringIdentifier, text))
        } catch (e: UnsupportedEncodingException) {
            e.printStackTrace()
        }
        val chatMessage = ChatMessage(text, MemberData("other", "red"), false)
        runOnUiThread {
            messageAdapter!!.add(chatMessage)
            messagesView!!.setSelection(messagesView!!.count - 1)
        }
    }

    fun showInstanceSearchDialog() {
        val confirm = AlertDialog.Builder(this@MainActivity)
                .setTitle("Hype started...")
                .setMessage("Looking for instances")
                .setIcon(android.R.drawable.ic_dialog_alert)
        dialog = confirm.create()
        dialog!!.show()
    }

    fun showSentFailedDialog(instance: Instance) {
        val confirm = AlertDialog.Builder(this@MainActivity)
                .setTitle("Sending Failed")
                .setMessage(String.format("Could not send to %s", instance.stringIdentifier))
                .setPositiveButton(android.R.string.yes) { dialog, which ->
                    Log.i(TAG, "Hype sent failed dialog clicked")
                    //                        setResolveDialogIsOpen(false);
                }
                .setIcon(android.R.drawable.ic_dialog_alert)
        val dialog = confirm.create()
        dialog.show()
    }

    override fun onHypeMessageFailedSending(messageInfo: MessageInfo, instance: Instance, error: Error) {
        Log.i(TAG, String.format("Hype message failed sending %s %s [%s]", messageInfo.identifier, instance.stringIdentifier, error.toString()))
        runOnUiThread { showSentFailedDialog(instance) }
    }

    override fun onHypeMessageSent(messageInfo: MessageInfo, instance: Instance, v: Float, b: Boolean) {
        Log.i(TAG, String.format("Hype message sent %s %s [%f] %b", messageInfo.identifier, instance.stringIdentifier, v, b))
    }

    override fun onHypeMessageDelivered(messageInfo: MessageInfo, instance: Instance, v: Float, b: Boolean) {
        Log.i(TAG, String.format("Hype message delivered %s %s [%f] %b", messageInfo.identifier, instance.stringIdentifier, v, b))
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

    override fun onHypeInstanceLost(instance: Instance, error: Error) {
        Log.i(TAG, String.format("Hype lost instance: %s [%s]", instance.stringIdentifier, error.toString()))
        // This instance is no longer available for communicating. If the instance
        // is somehow being tracked, such as by a map of instances, this would be
        // the proper time for cleanup.
        runOnUiThread { showInstanceSearchDialog() }
    }

    fun showResolveDialog(instance: Instance) {
        setResolveDialogIsOpen(true)
        val confirm = AlertDialog.Builder(this@MainActivity)
                .setTitle("New Instance Resolved")
                .setMessage(String.format("Instance found: %s\nDo you wish to communicate?", instance.stringIdentifier))
                .setPositiveButton(android.R.string.yes) { dialog, which ->
                    Log.d(TAG, "Hype will communicate with instance")
                    setResolveDialogIsOpen(false)
                }
                .setNegativeButton(android.R.string.no) { dialogInterface, i -> setResolveDialogIsOpen(false) }
                .setIcon(android.R.drawable.ic_dialog_alert)
        val dialog = confirm.create()
        dialog.show()
    }

    override fun onHypeInstanceResolved(instance: Instance) {
        Log.i(TAG, String.format("Hype resolved instance: %s", instance.stringIdentifier))
        // At this point the instance is ready to communicate. Sending and receiving
        // content is possible at any time now.
        resolvedInstance = instance
        if (!isResolveDialogOpen) {
            runOnUiThread {
                if (dialog != null) {
                    dialog!!.dismiss()
                }
                showResolveDialog(instance)
            }
        }
    }

    override fun onHypeInstanceFailResolving(instance: Instance, error: Error) {
        Log.i(TAG, String.format("Hype could not resolve instance: %s [%s]", instance.stringIdentifier, error.toString()))
    }

    override fun onHypeStart() {
        Log.i(TAG, "Hype started")
        runOnUiThread { showInstanceSearchDialog() }
    }

    override fun onHypeStop(error: Error) {
        if (error != null) {
            Log.i(TAG, String.format("Hype stopped [%s]", error.toString()))
        } else {
            Log.i(TAG, "Hype stopped [null]. You should do reinstall the application.")
        }
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
            } catch (e: UnsupportedEncodingException) {
                e.printStackTrace()
            }
            val mData = MemberData("Bobby", "")
            val m = ChatMessage(message, mData, true)
            runOnUiThread {
                messageAdapter!!.add(m)
                messagesView!!.setSelection(messagesView!!.count - 1)
            }
            editText!!.text.clear()
        }
    }

    override fun onOpen(room: Room) {
        println("Connected to room")
    }

    override fun onOpenFailure(room: Room, ex: Exception) {
        System.err.println(ex)
    }

    override fun onMessage(room: Room, receivedMessage: Message) {
        val mapper = ObjectMapper()
        try {
            val data = mapper.treeToValue(receivedMessage.member.clientData, MemberData::class.java)
            val belongsToCurrentUser = receivedMessage.clientID == scaledrone!!.clientID
            val message = ChatMessage(receivedMessage.data.asText(), data, belongsToCurrentUser)
            runOnUiThread {
                messageAdapter!!.add(message)
                messagesView!!.setSelection(messagesView!!.count - 1)
            }
        } catch (e: JsonProcessingException) {
            e.printStackTrace()
        }
    }

    private val randomName: String
        private get() {
            val adjs = arrayOf("autumn", "hidden", "bitter", "misty", "silent", "empty", "dry", "dark", "summer", "icy", "delicate", "quiet", "white", "cool", "spring", "winter", "patient", "twilight", "dawn", "crimson", "wispy", "weathered", "blue", "billowing", "broken", "cold", "damp", "falling", "frosty", "green", "long", "late", "lingering", "bold", "little", "morning", "muddy", "old", "red", "rough", "still", "small", "sparkling", "throbbing", "shy", "wandering", "withered", "wild", "black", "young", "holy", "solitary", "fragrant", "aged", "snowy", "proud", "floral", "restless", "divine", "polished", "ancient", "purple", "lively", "nameless")
            val nouns = arrayOf("waterfall", "river", "breeze", "moon", "rain", "wind", "sea", "morning", "snow", "lake", "sunset", "pine", "shadow", "leaf", "dawn", "glitter", "forest", "hill", "cloud", "meadow", "sun", "glade", "bird", "brook", "butterfly", "bush", "dew", "dust", "field", "fire", "flower", "firefly", "feather", "grass", "haze", "mountain", "night", "pond", "darkness", "snowflake", "silence", "sound", "sky", "shape", "surf", "thunder", "violet", "water", "wildflower", "wave", "water", "resonance", "sun", "wood", "dream", "cherry", "tree", "fog", "frost", "voice", "paper", "frog", "smoke", "star")
            return adjs[Math.floor(Math.random() * adjs.size).toInt()] +
                    "_" +
                    nouns[Math.floor(Math.random() * nouns.size).toInt()]
        }

    private val randomColor: String
        private get() {
            val r = Random()
            val sb = StringBuffer("#")
            while (sb.length < 7) {
                sb.append(Integer.toHexString(r.nextInt()))
            }
            return sb.toString().substring(0, 7)
        }

    fun setResolveDialogIsOpen(resolveDialogIsOpen: Boolean) {
        isResolveDialogOpen = resolveDialogIsOpen
    }

    companion object {
        // replace this with a real channelID from Scaledrone dashboard
        private val TAG = MainActivity::class.java.name
    }
}

internal class MemberData {
    var name: String? = null
        private set
    var color: String? = null
        private set

    constructor(name: String?, color: String?) {
        this.name = name
        this.color = color
    }

    constructor() {}

    override fun toString(): String {
        return "MemberData{" +
                "name='" + name + '\'' +
                ", color='" + color + '\'' +
                '}'
    }
}