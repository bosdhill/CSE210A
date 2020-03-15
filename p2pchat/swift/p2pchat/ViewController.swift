import UIKit
import MessageKit
import Hype

class ViewController: MessagesViewController, HYPStateObserver, HYPNetworkObserver, HYPMessageObserver  {
    
    var messages: [Message] = []
    var member: Member!
    var resolvedInstance: HYPInstance!
    var instanceSearchController: UIAlertController!
    var didResolveController: UIAlertController!
    var dialog: Dialog = Dialog()
    var RESOLVED_INSTANCE_TITLE: String = "Hype new instance resolved"
    var SEARCH_INSTANCE_TITLE: String = "Hype started..."
    var SEARCH_INSTANCE_BODY: String = "Searching for instances"
    var NO_INSTANCE_TITLE: String = "No resolved instance"
    var NO_INSTANCE_BODY: String = "Would you like to search for an instance?"
    var SENT_FAILED_TITLE: String = "Sending Failed"
    var SENT_TITLE: String = "Sending message..."
    var RECV_TITLE: String = "Delivered"
    var FAILED_STARTING_TITLE: String = "Hype failed starting"
    var ANIMATED: Bool = false
    
    override func viewDidLoad() {
        super.viewDidLoad()
        messagesCollectionView.messagesDataSource = self
        messagesCollectionView.messagesLayoutDelegate = self
        messageInputBar.delegate = self
        messagesCollectionView.messagesDisplayDelegate = self
        requestHypeToStart()
    }
    
    override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
    }
    
    func requestHypeToStart() -> () {
        HYP.add(self as HYPStateObserver)
        HYP.add(self as HYPNetworkObserver)
        HYP.add(self as HYPMessageObserver)
        HYP.setAppIdentifier("c990ae8f")
        HYP.start()
    }
    
    func hypeDidStart() {
        NSLog("Hype started!")
        DispatchQueue.main.async {
            self.present(self.dialog.show(title: self.SEARCH_INSTANCE_TITLE, message: self.SEARCH_INSTANCE_BODY, handler: nil),
                         animated: self.ANIMATED, completion: nil)
        }
    }
    
    func hypeDidStopWithError(_ error: HYPError!) {
        let description:String! = error == nil ? "" : error.description
        NSLog("Hype stopped [%@]", description)
    }
    
    func hypeDidFailStartingWithError(_ error: HYPError!) {
        NSLog("Hype failed starting [%@]", error.description)
        NSLog("Hype code [%d]", error.code.rawValue)
        NSLog("Hype suggestion [%@]", error.suggestion)
        let message = String(format: "Error: [%s]", error.description)
        DispatchQueue.main.async {
            self.present(self.dialog.show(title: self.FAILED_STARTING_TITLE, message: message, handler: nil),
                         animated: self.ANIMATED, completion: nil)
        }
    }
    
    func hypeDidChangeState() {
        switch HYP.state().rawValue {
        case 0: NSLog("Hype is in idle state")
        case 1: NSLog("Hype is in starting state")
        case 2: NSLog("Hype is in running state")
        case 3: NSLog("Hype is in stopping state")
        default: break
        }
    }
    
    func hypeDidBecomeReady() {
        NSLog("Hype is ready")
        // Where're here due to a failed start request, try again
        requestHypeToStart()
    }
    
    func hypeDidRequestAccessToken(withUserIdentifier userIdentifier: UInt) -> String! {
        return "3905669394fa2533"
    }
    
    func hypeDidFind(_ instance: HYPInstance!) {
        NSLog("Hype did find instance %@", instance.appStringIdentifier!)
    }
    
    func hypeDidLose(_ instance: HYPInstance!, error: HYPError!) {
        NSLog("Hype did lost instance %@ [%s]", instance.appStringIdentifier!, error.description)
        let message = String(format: "Lost instance: %@", instance.appStringIdentifier!)
        resolvedInstance = nil
        DispatchQueue.main.async {
            self.present(self.dialog.show(title: self.NO_INSTANCE_TITLE, message: message, handler: nil),
                         animated: true, completion: nil)
        }
    }
    
    // example of closure in Swift
    func resolveHandler(instance: HYPInstance!) -> (_ alertAction:UIAlertAction) -> () {
        return {_ in
            self.resolvedInstance = instance
            NSLog("Hype will communicate with instance %@", instance.appStringIdentifier!)
        }
    }
    
    func hypeDidResolve(_ instance: HYPInstance!) {
        NSLog("Hype resolved instance: %@", instance.stringIdentifier!)
        let message = String(format: "Instance found: %@\nDo you wish to communicate?", instance.stringIdentifier!)
        DispatchQueue.main.async {
            self.present(self.dialog.show(title: self.RESOLVED_INSTANCE_TITLE, message: message,
                                          handler: self.resolveHandler(instance: instance)), animated: self.ANIMATED, completion: nil)
        }
    }
    
    func hypeDidFailResolving(_ instance: HYPInstance!, error: HYPError!) {
        NSLog("Hype did fail resolving instance %@ [%s]", instance.appStringIdentifier!, error.description)
    }
    
    func hypeDidReceive(_ message: HYPMessage!, from fromInstance: HYPInstance!) {
        NSLog("Hype did receive %d %@", message.info.identifier, fromInstance.appStringIdentifier!)
        let msg = (NSString(data: (message?.data)!, encoding: String.Encoding.utf8.rawValue)! as String)
        NSLog("Hype msg recieved [%@]", msg)
        DispatchQueue.main.async {
            self.messages.append(Message(member: Member(name: "Shlobby", color:
                UIColor(displayP3Red: 1.0, green: 1.0, blue: 0.0, alpha: 1.0)), text: msg, messageId: "32"))
            self.messagesCollectionView.reloadData()
            self.messagesCollectionView.scrollToBottom(animated: true)
        }
    }
    
    func hypeDidFailSendingMessage(_ messageInfo: HYPMessageInfo!, to toInstance: HYPInstance!, error: HYPError!) {
        NSLog("Hype did fail sending  %d %@ %s", messageInfo.identifier, toInstance.appStringIdentifier!, error.description)
        let message = String(format: "Could not send to %@", toInstance.appStringIdentifier!)
        DispatchQueue.main.async {
            self.present(self.dialog.show(title: self.SENT_FAILED_TITLE, message: message, handler: nil), animated: self.ANIMATED, completion: nil)
        }
    }
    
    func hypeDidDeliverMessage(_ messageInfo: HYPMessageInfo!, to toInstance: HYPInstance!, progress: Float, complete: Bool) {
        if complete {
            NSLog("Hype delivered message %d %@ %f", messageInfo.identifier, toInstance.appStringIdentifier!, progress)
            DispatchQueue.main.async {
                self.present(self.dialog.show(title: self.RECV_TITLE, message: "", handler: nil), animated: self.ANIMATED, completion: nil)
            }
        }
    }
}

extension ViewController: MessagesDataSource {
    func numberOfSections(in messagesCollectionView: MessagesCollectionView) -> Int {
        return messages.count
    }
    
    func currentSender() -> Sender {
        return Sender(id: "Bobby", displayName: "Bobby")
    }
    
    func messageForItem(at indexPath: IndexPath,
                        in messagesCollectionView: MessagesCollectionView) -> MessageType {
        
        return messages[indexPath.section]
    }
    
    func messageTopLabelHeight(for message: MessageType, at indexPath: IndexPath, in messagesCollectionView: MessagesCollectionView) -> CGFloat {
        return 12
    }
    
    func messageTopLabelAttributedText(for message: MessageType, at indexPath: IndexPath) -> NSAttributedString? {
        return NSAttributedString(
            string: message.sender.displayName,
            attributes: [.font: UIFont.systemFont(ofSize: 12)])
    }
}

extension ViewController: MessagesLayoutDelegate {
    func heightForLocation(message: MessageType,
                           at indexPath: IndexPath,
                           with maxWidth: CGFloat,
                           in messagesCollectionView: MessagesCollectionView) -> CGFloat {
        return 0
    }
}

extension ViewController: MessagesDisplayDelegate {
    func configureAvatarView(
        _ avatarView: AvatarView,
        for message: MessageType,
        at indexPath: IndexPath,
        in messagesCollectionView: MessagesCollectionView) {
        
        let message = messages[indexPath.section]
        let color = message.member.color
        avatarView.backgroundColor = color
    }
}

extension ViewController: MessageInputBarDelegate {
    func messageInputBar(_ inputBar: MessageInputBar, didPressSendButtonWith text: String) {
        sendMessage(text: text)
        inputBar.inputTextView.text = ""
    }
    
    func noInstanceHandler(_ alertAction:UIAlertAction) -> () {
        NSLog("Hype no instance yes clicked")
        return self.requestHypeToStart()
    }
    
    func sendMessage(text: String) {
        let col = UIColor(displayP3Red: 1.0, green: 0.0, blue: 0.0, alpha: 1.0)
        let m = Member(name: "Bobby", color: col)
        let chatMessage = Message(
            member: m,
            text: text,
            messageId: UUID().uuidString)
        
        let data: Data? = text.data(using: String.Encoding.utf8)
        
        if self.resolvedInstance != nil {
            DispatchQueue.main.async {
                self.present(self.dialog.show(title: self.SENT_TITLE, message: "", handler: nil),
                             animated: self.ANIMATED, completion: nil)
            }
            HYP.send(data, to: self.resolvedInstance, trackProgress: true)
            showMessage(message: chatMessage)
        }
        else {
            DispatchQueue.main.async {
                self.present(self.dialog.show(title: self.NO_INSTANCE_TITLE, message: self.NO_INSTANCE_BODY, handler: self.noInstanceHandler),
                             animated: self.ANIMATED, completion: nil)
            }
        }
    }
    
    func showMessage(message: Message) {
        DispatchQueue.main.async {
            self.messages.append(message)
            self.messagesCollectionView.reloadData()
            self.messagesCollectionView.scrollToBottom(animated: true)
        }
    }
}

