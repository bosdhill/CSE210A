package com.p2p.chat;

import android.content.DialogInterface;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.EditText;
import android.widget.ListView;

import android.util.Log;

import com.hypelabs.hype.Error;
import com.hypelabs.hype.Hype;
import com.hypelabs.hype.Instance;
import com.hypelabs.hype.Message;
import com.hypelabs.hype.MessageInfo;
import com.hypelabs.hype.MessageObserver;
import com.hypelabs.hype.NetworkObserver;
import com.hypelabs.hype.StateObserver;

import java.io.UnsupportedEncodingException;

public class MainActivity extends AppCompatActivity implements StateObserver, NetworkObserver, MessageObserver {

    private static String TAG = MainActivity.class.getName();
    private EditText editText;
    private ChatMessageAdapter messageAdapter;
    private ListView messagesView;
    private Instance resolvedInstance;
    private ChatMessage sentMessage;
    private Dialog dialog = new Dialog();
    private final String RESOLVED_INSTANCE_TITLE = "Hype new instance resolved";
    private final String SEARCH_INSTANCE_TITLE = "Hype started...";
    private final String SEARCH_INSTANCE_BODY = "Searching for instances";
    private final String NO_INSTANCE_TITLE = "No resolved instance";
    private final String NO_INSTANCE_BODY = "Would you like to search for an instance?";
    private final String SENT_FAILED_TITLE = "Sending Failed";
    private final String SENT_TITLE = "Sending message...";
    private final String RECV_TITLE = "Delivered";
    private final String FAILED_STARTING_TITLE = "Hype failed starting";
    private final String LOST_INSTANCE_TITLE = "Hype instance lost";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        editText = (EditText) findViewById(R.id.editText);

        messageAdapter = new ChatMessageAdapter(this);
        messagesView = (ListView) findViewById(R.id.messages_view);
        messagesView.setAdapter(messageAdapter);
        requestHypeToStart();
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

    private void displayMessage(ChatMessage message) {
        if (message != null) {
            messageAdapter.add(message);
            messagesView.setSelection(messagesView.getCount() - 1);
        }
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
        final ChatMessage chatMessage = new ChatMessage(message, new MemberData("Bobby","red") , false);
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                displayMessage(chatMessage);
            }
        });
    }

    @Override
    public void onHypeMessageFailedSending(MessageInfo messageInfo, Instance instance, Error error) {
        Log.i(TAG, String.format("Hype message failed sending %s %s [%s]", messageInfo.getIdentifier(), instance.getStringIdentifier(),  error.toString()));
        final Instance inst = instance;
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                dialog.show(MainActivity.this, findNewInstance(), SENT_FAILED_TITLE,
                        String.format("Could not send to %s.\n" + NO_INSTANCE_BODY, inst.getStringIdentifier()), false);
            }
        });
    }

    @Override
    public void onHypeMessageSent(MessageInfo messageInfo, Instance instance, float v, boolean b) {
        Log.i(TAG, String.format("Hype message sent %s %s [%f] %b", messageInfo.getIdentifier(), instance.getStringIdentifier(), v, b));
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                dialog.show(MainActivity.this, null, SENT_TITLE, "", false);
            }
        });
    }

    @Override
    public void onHypeMessageDelivered(MessageInfo messageInfo, Instance instance, float v, boolean b) {
        Log.i(TAG, String.format("Hype message delivered %s %s [%f] %b", messageInfo.getIdentifier(), instance.getStringIdentifier(), v, b));
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                dialog.show(MainActivity.this, null, RECV_TITLE, "", true);
                displayMessage(sentMessage);
            }
        });
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

    private DialogInterface.OnClickListener findNewInstance() {
        DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialogInterface, int i) {
                Log.i(TAG, "Hype no instance yes clicked");
                requestHypeToStart();
            }
        };
        return listener;
    }

    @Override
    public void onHypeInstanceLost(Instance instance, Error error) {
        Log.i(TAG, String.format("Hype lost instance: %s [%s]", instance.getStringIdentifier(), error.toString()));
        // This instance is no longer available for communicating. If the instance
        // is somehow being tracked, such as by a map of instances, this would be
        // the proper time for cleanup.
        resolvedInstance = null;
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                dialog.show(MainActivity.this, findNewInstance(),
                        LOST_INSTANCE_TITLE, NO_INSTANCE_BODY, false);
            }
        });
    }

    private DialogInterface.OnClickListener setNewInstance(final Instance instance) {
        DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialogInterface, int i) {
                resolvedInstance = instance;
            }
        };
        return listener;
    }

    @Override
    public void onHypeInstanceResolved(final Instance instance) {
        Log.i(TAG, String.format("Hype resolved instance: %s", instance.getStringIdentifier()));
        // At this point the instance is ready to communicate. Sending and receiving
        // content is possible at any time now.
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
            dialog.show(MainActivity.this, setNewInstance(instance), RESOLVED_INSTANCE_TITLE,
                    String.format("Instance found: %s\nDo you wish to communicate?", instance.getStringIdentifier()), false);
            }
        });
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
                dialog.show(MainActivity.this, null,
                        SEARCH_INSTANCE_TITLE, SEARCH_INSTANCE_BODY, false);
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
        final Error err = error;
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                dialog.show(MainActivity.this, null,
                        FAILED_STARTING_TITLE, String.format("Error: [%s]", err.toString()), true);
            }
        });
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
            try {
             Message m = sendMessage(message, resolvedInstance, true);
             sentMessage = new ChatMessage(m, new MemberData("Bobby", ""), true);
             editText.getText().clear();
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
            catch (Exception e) {
                runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                    dialog.show(MainActivity.this, findNewInstance(), NO_INSTANCE_TITLE,
                            NO_INSTANCE_BODY, false);
                    }
                });
            }
        }
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
