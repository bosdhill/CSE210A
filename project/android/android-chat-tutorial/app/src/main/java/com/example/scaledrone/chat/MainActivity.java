package com.example.scaledrone.chat;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.EditText;
import android.widget.ListView;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import android.util.Log;

import com.hypelabs.hype.Error;
import com.hypelabs.hype.Hype;
import com.hypelabs.hype.Instance;
import com.hypelabs.hype.Message;
import com.hypelabs.hype.MessageInfo;
import com.hypelabs.hype.MessageObserver;
import com.hypelabs.hype.NetworkObserver;
import com.hypelabs.hype.StateObserver;
import com.scaledrone.lib.Listener;
import com.scaledrone.lib.Room;
import com.scaledrone.lib.RoomListener;
import com.scaledrone.lib.Scaledrone;

import java.io.UnsupportedEncodingException;
import java.util.Random;

public class MainActivity extends AppCompatActivity implements RoomListener, StateObserver, NetworkObserver, MessageObserver {

    // replace this with a real channelID from Scaledrone dashboard
    private static String TAG = MainActivity.class.getName();
    private String channelID = "CHANNEL_ID_FROM_YOUR_SCALEDRONE_DASHBOARD";
    private String roomName = "observable-room";
    private EditText editText;
    private Scaledrone scaledrone;
    private Communicator communicator;
    private ChatMessageAdapter messageAdapter;
    private ListView messagesView;
    private Instance resolvedInstance;
    private boolean attached;
    private boolean resolveDialogIsOpen = false;
    private AlertDialog dialog;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        editText = (EditText) findViewById(R.id.editText);

        messageAdapter = new ChatMessageAdapter(this);
        messagesView = (ListView) findViewById(R.id.messages_view);
        messagesView.setAdapter(messageAdapter);

        MemberData data = new MemberData(getRandomName(), getRandomColor());

        requestHypeToStart();
        scaledrone = new Scaledrone(channelID, data);
        scaledrone.connect(new Listener() {
            @Override
            public void onOpen() {
                System.out.println("Scaledrone connection open");
                scaledrone.subscribe(roomName, MainActivity.this);
            }

            @Override
            public void onOpenFailure(Exception ex) {
                System.err.println(ex);
            }

            @Override
            public void onFailure(Exception ex) {
                System.err.println(ex);
            }

            @Override
            public void onClosed(String reason) {
                System.err.println(reason);
            }
        });
    }

    protected void requestHypeToStart() {
        Log.i(TAG, String.format("requestHypeToStart"));
        // The application context is used to query the user for permissions, such as using
        // the Bluetooth adapter or enabling Wi-Fi. The context must be set before anything
        // else is attempted, otherwise resulting in an exception being thrown.
        Hype.setContext(getApplicationContext());

        // Adding itself as an Hype state observer makes sure that the application gets
        // notifications for lifecycle events being triggered by the Hype SDK. These
        // events include starting and stopping, as well as some error handling.
        Hype.addStateObserver(this);

        // Need to implement this to announce to other user
        Hype.setAnnouncement("New user found".getBytes());

        // Network observer notifications include other devices entering and leaving the
        // network. When a device is found all observers get a onHypeInstanceFound
        // notification, and when they leave onHypeInstanceLost is triggered instead.
        // This observer also gets notifications for onHypeInstanceResolved when an
        // instance is resolved.
        Hype.addNetworkObserver(this);

        // Message notifications indicate when messages are received, sent, or delivered.
        // Such callbacks are called with progress tracking indication.
        Hype.addMessageObserver(this);

        // App identifiers are used to segregate the network. Apps with different identifiers
        // do not communicate with each other, although they still cooperate on the network.
        Hype.setAppIdentifier("c990ae8f");

        // Requesting Hype to start is equivalent to requesting the device to publish
        // itself on the network and start browsing for other devices in proximity. If
        // everything goes well, the onHypeStart() observer method gets called, indicating
        // that the device is actively participating on the network.
        Hype.start();
    }

    @Override
    public void onHypeMessageReceived(Message message, Instance instance) {
        Log.i(TAG, String.format("Hype message received %s %s", message.getIdentifier(), instance.getStringIdentifier()));
        String text = null;
        try {
            text = new String(message.getData(), "UTF-8");
            // If all goes well, this will log the original text
            Log.i(TAG, String.format("Hype received a message from: %s %s", instance.getStringIdentifier(), text));
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        final ChatMessage chatMessage = new ChatMessage(text, new MemberData("other","red") , false);
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                messageAdapter.add(chatMessage);
                messagesView.setSelection(messagesView.getCount() - 1);
            }
        });
    }

    public void showInstanceSearchDialog() {
        final AlertDialog.Builder confirm = new AlertDialog.Builder(MainActivity.this)
                .setTitle("Hype started...")
                .setMessage("Looking for instances")
                .setIcon(android.R.drawable.ic_dialog_alert);
        dialog = confirm.create();
        dialog.show();
    }

    public void showSentFailedDialog(Instance instance) {
        final AlertDialog.Builder confirm = new AlertDialog.Builder(MainActivity.this)
                .setTitle("Sending Failed")
                .setMessage(String.format("Could not send to %s", instance.getStringIdentifier()))
                .setPositiveButton(android.R.string.yes, new DialogInterface.OnClickListener() {
                    public void onClick(DialogInterface dialog, int which) {
                        Log.i(TAG, "Hype sent failed dialog clicked");
//                        setResolveDialogIsOpen(false);
                    }
                })
                .setIcon(android.R.drawable.ic_dialog_alert);
        final AlertDialog dialog = confirm.create();
        dialog.show();
    }

    @Override
    public void onHypeMessageFailedSending(MessageInfo messageInfo, Instance instance, Error error) {
        Log.i(TAG, String.format("Hype message failed sending %s %s [%s]", messageInfo.getIdentifier(), instance.getStringIdentifier(),  error.toString()));
        final Instance inst = instance;
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                showSentFailedDialog(inst);
            }
        });
    }

    @Override
    public void onHypeMessageSent(MessageInfo messageInfo, Instance instance, float v, boolean b) {
        Log.i(TAG, String.format("Hype message sent %s %s [%f] %b", messageInfo.getIdentifier(), instance.getStringIdentifier(), v, b));
    }

    @Override
    public void onHypeMessageDelivered(MessageInfo messageInfo, Instance instance, float v, boolean b) {
        Log.i(TAG, String.format("Hype message delivered %s %s [%f] %b", messageInfo.getIdentifier(), instance.getStringIdentifier(), v, b));
    }

    boolean shouldResolveInstance(Instance instance) {
        // This method should decide whether an instance is interesting for communicating.
        // For that purpose, the implementation could use instance.userIdentifier, but it's
        // noticeable that announcements may not be available yet. Announcements are only
        // exchanged during the handshake.
        return true;
    }

    @Override
    public void onHypeInstanceFound(Instance instance) {
        Log.i(TAG, "found instance: " + instance.getAppStringIdentifier());
        // Instances need to be resolved before being ready for communicating. This will
        // force the two of them to perform an handshake.
        if (shouldResolveInstance(instance)) {
            Hype.resolve(instance);
        }
    }

    @Override
    public void onHypeInstanceLost(Instance instance, Error error) {
        Log.i(TAG, String.format("Hype lost instance: %s [%s]", instance.getStringIdentifier(), error.toString()));
        // This instance is no longer available for communicating. If the instance
        // is somehow being tracked, such as by a map of instances, this would be
        // the proper time for cleanup.
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                showInstanceSearchDialog();
            }
        });
    }

    public void showResolveDialog(Instance instance) {
        setResolveDialogIsOpen(true);
        final AlertDialog.Builder confirm = new AlertDialog.Builder(MainActivity.this)
                .setTitle("New Instance Resolved")
                .setMessage(String.format("Instance found: %s\nDo you wish to communicate?", instance.getStringIdentifier()))
                .setPositiveButton(android.R.string.yes, new DialogInterface.OnClickListener() {
                    public void onClick(DialogInterface dialog, int which) {
                        Log.d(TAG, "Hype will communicate with instance");
                        setResolveDialogIsOpen(false);
                    }
                })
                .setNegativeButton(android.R.string.no, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialogInterface, int i) {
                        setResolveDialogIsOpen(false);
                    }
                })
                .setIcon(android.R.drawable.ic_dialog_alert);
        final AlertDialog dialog = confirm.create();
        dialog.show();
    }

    @Override
    public void onHypeInstanceResolved(final Instance instance) {
        Log.i(TAG, String.format("Hype resolved instance: %s", instance.getStringIdentifier()));
        // At this point the instance is ready to communicate. Sending and receiving
        // content is possible at any time now.
        this.resolvedInstance = instance;
        if (!isResolveDialogOpen()) {
            runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    if (dialog != null) {
                        dialog.dismiss();
                    }
                    showResolveDialog(instance);
                }
            });
        }
    }

    @Override
    public void onHypeInstanceFailResolving(Instance instance, Error error) {
        Log.i(TAG, String.format("Hype could not resolve instance: %s [%s]", instance.getStringIdentifier(), error.toString()));
    }

    @Override
    public void onHypeStart() {
        Log.i(TAG, "Hype started");
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                showInstanceSearchDialog();
            }
        });
    }

    @Override
    public void onHypeStop(Error error) {
        if (error != null) {
            Log.i(TAG, String.format("Hype stopped [%s]", error.toString()));
        }
        else {
            Log.i(TAG, "Hype stopped [null]. You should do reinstall the application.");
        }
    }

    @Override
    public void onHypeFailedStarting(Error error) {
        Log.i(TAG, String.format("Hype failed starting [%s]", error.toString()));
    }


    @Override
    public void onHypeReady() {
        Log.i(TAG, String.format("Hype is ready"));
    }

    @Override
    public void onHypeStateChange() {
        switch(Hype.getState()) {
            case Starting:
                Log.i(TAG, "Hype is in starting state");
                break;
            case Idle:
                Log.i(TAG, "Hype is in idle state");
                break;
            case Running:
                Log.i(TAG, "Hype is in running state");
                break;
            case Stopping:
                Log.i(TAG, "Hype is in stopping state");
                break;
        }
    }

    @Override
    public String onHypeRequestAccessToken(int userIdentifier) {
        return "3905669394fa2533";
    }

    protected Message sendMessage(String text, Instance instance, boolean acknowledge) throws UnsupportedEncodingException {

        // When sending content there must be some sort of protocol that both parties
        // understand. In this case, we simply send the text encoded in UTF-8. The data
        // must be decoded when received, using the same encoding.
        byte[] data = text.getBytes("UTF-8");

        return Hype.send(data, instance, acknowledge);
    }

    public void sendMessage(View view) {
        System.out.println("sendMessage");
        String message = editText.getText().toString();
        if (message.length() > 0) {
            Message sentMessage;
            try {
             sentMessage = sendMessage(message, resolvedInstance, true);
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
            final MemberData mData = new MemberData("Bobby", "");
            final ChatMessage m = new ChatMessage(message, mData, true);
            runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    messageAdapter.add(m);
                    messagesView.setSelection(messagesView.getCount() - 1);
                }
            });
            editText.getText().clear();
        }
    }

    @Override
    public void onOpen(Room room) {
        System.out.println("Connected to room");
    }

    @Override
    public void onOpenFailure(Room room, Exception ex) {
        System.err.println(ex);
    }

    @Override
    public void onMessage(Room room, com.scaledrone.lib.Message receivedMessage) {
        final ObjectMapper mapper = new ObjectMapper();
        try {
            final MemberData data = mapper.treeToValue(receivedMessage.getMember().getClientData(), MemberData.class);
            boolean belongsToCurrentUser = receivedMessage.getClientID().equals(scaledrone.getClientID());
            final ChatMessage message = new ChatMessage(receivedMessage.getData().asText(), data, belongsToCurrentUser);
            runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    messageAdapter.add(message);
                    messagesView.setSelection(messagesView.getCount() - 1);
                }
            });
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }
    }

    private String getRandomName() {
        String[] adjs = {"autumn", "hidden", "bitter", "misty", "silent", "empty", "dry", "dark", "summer", "icy", "delicate", "quiet", "white", "cool", "spring", "winter", "patient", "twilight", "dawn", "crimson", "wispy", "weathered", "blue", "billowing", "broken", "cold", "damp", "falling", "frosty", "green", "long", "late", "lingering", "bold", "little", "morning", "muddy", "old", "red", "rough", "still", "small", "sparkling", "throbbing", "shy", "wandering", "withered", "wild", "black", "young", "holy", "solitary", "fragrant", "aged", "snowy", "proud", "floral", "restless", "divine", "polished", "ancient", "purple", "lively", "nameless"};
        String[] nouns = {"waterfall", "river", "breeze", "moon", "rain", "wind", "sea", "morning", "snow", "lake", "sunset", "pine", "shadow", "leaf", "dawn", "glitter", "forest", "hill", "cloud", "meadow", "sun", "glade", "bird", "brook", "butterfly", "bush", "dew", "dust", "field", "fire", "flower", "firefly", "feather", "grass", "haze", "mountain", "night", "pond", "darkness", "snowflake", "silence", "sound", "sky", "shape", "surf", "thunder", "violet", "water", "wildflower", "wave", "water", "resonance", "sun", "wood", "dream", "cherry", "tree", "fog", "frost", "voice", "paper", "frog", "smoke", "star"};
        return (
            adjs[(int) Math.floor(Math.random() * adjs.length)] +
            "_" +
            nouns[(int) Math.floor(Math.random() * nouns.length)]
        );
    }

    private String getRandomColor() {
        Random r = new Random();
        StringBuffer sb = new StringBuffer("#");
        while(sb.length() < 7){
            sb.append(Integer.toHexString(r.nextInt()));
        }
        return sb.toString().substring(0, 7);
    }

    public boolean isAttached() {
        return attached;
    }

    public void setAttached(boolean attached) {
        this.attached = attached;
    }

    public boolean isResolveDialogOpen() {
        return resolveDialogIsOpen;
    }

    public void setResolveDialogIsOpen(boolean resolveDialogIsOpen) {
        this.resolveDialogIsOpen = resolveDialogIsOpen;
    }
}

class MemberData {
    private String name;
    private String color;

    public MemberData(String name, String color) {
        this.name = name;
        this.color = color;
    }

    public MemberData() {
    }

    public String getName() {
        return name;
    }

    public String getColor() {
        return color;
    }

    @Override
    public String toString() {
        return "MemberData{" +
                "name='" + name + '\'' +
                ", color='" + color + '\'' +
                '}';
    }
}
