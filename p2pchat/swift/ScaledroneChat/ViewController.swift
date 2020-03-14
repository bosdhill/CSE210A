import UIKit
import MessageKit
import Hype

class ViewController: MessagesViewController, HYPStateObserver, HYPNetworkObserver, HYPMessageObserver  {
    
  var messages: [Message] = []
  var member: Member!
  var resolvedInstance: HYPInstance!
  var instanceSearchController: UIAlertController!
  var didResolveController: UIAlertController!
  private var instanceSearchView: UIView!
  
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
    
    func showSearchDialog(title: String, message: String) {
        if instanceSearchController == nil {
            instanceSearchController = UIAlertController(title: title, message: message, preferredStyle: .alert)
            present(instanceSearchController, animated: true, completion: nil)
        }
    }
    
    func hideSearchDialog() {
        if instanceSearchController != nil {
            instanceSearchController.dismiss(animated: true, completion: nil)
        }
    }
    
    // example of currying in Swift
    func handleConfirmPressed(instance: HYPInstance!) -> (_ alertAction:UIAlertAction) -> () {
        return {_ in
            self.resolvedInstance = instance
            NSLog("Hype will communicate with instance %@", instance.appStringIdentifier!)
        }
    }
    
    func showDidResolveDialog(title: String, message: String, instance: HYPInstance!) {
        if didResolveController == nil {
            didResolveController = UIAlertController(title: title, message: message, preferredStyle: .alert)
            didResolveController.addAction(UIAlertAction(title: "OK", style: .default, handler: handleConfirmPressed(instance: instance)))
            didResolveController.addAction(UIAlertAction(title: "CANCEL", style: .cancel, handler: nil))
            present(didResolveController, animated: true, completion: nil)
        }
    }
    
    func hideDidResolveDialog() {
        if didResolveController != nil {
            didResolveController.dismiss(animated: true, completion: nil)
        }
    }
    
    func requestHypeToStart() {
        HYP.add(self as HYPStateObserver)
        HYP.add(self as HYPNetworkObserver)
        HYP.add(self as HYPMessageObserver)
        HYP.setAppIdentifier("c990ae8f")
        HYP.start()
    }

    func hypeDidStart() {
        NSLog("Hype started!")
        DispatchQueue.main.async {
            self.showSearchDialog(title: "Hype started", message: "Searching for instances...")
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
    }

    func hypeDidChangeState()
    {
        NSLog("Hype state changed to [%d] (Idle=0, Starting=1, Running=2, Stopping=3)", HYP.state().rawValue)
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
        DispatchQueue.main.async {
            self.showSearchDialog(title: "Hype instance lost", message: "Searching for instances...")
        }
    }

    func hypeDidResolve(_ instance: HYPInstance!)
    {
        NSLog("Hype resolved instance: %@", instance.stringIdentifier!)
        DispatchQueue.main.async {
            self.hideSearchDialog()
            self.showDidResolveDialog(title: "Hype instance found", message: instance.stringIdentifier, instance: instance)
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
            self.messages.append(Message(member: Member(name: "Shlobby", color: UIColor(displayP3Red: 1.0, green: 1.0, blue: 0.0, alpha: 1.0)), text: msg, messageId: "32"))
            self.messagesCollectionView.reloadData()
            self.messagesCollectionView.scrollToBottom(animated: true)
        }
    }

    func hypeDidFailSendingMessage(_ messageInfo: HYPMessageInfo!, to toInstance: HYPInstance!, error: HYPError!) {
        NSLog("Hype did fail sending  %d %@ %s", messageInfo.identifier, toInstance.appStringIdentifier!, error.description)
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
    func sendMessage(text: String) {
        let col = UIColor(displayP3Red: 1.0, green: 0.0, blue: 0.0, alpha: 1.0)
        let m = Member(name: "Bobby", color: col)
        let chatMessage = Message(
            member: m,
            text: text,
            messageId: UUID().uuidString)
        
        let data: Data? = text.data(using: String.Encoding.utf8)
        
        let _: HYPMessage? = HYP.send(data, to: self.resolvedInstance)
        messageCallback(message: chatMessage)
    }
    func messageCallback(message: Message) {
        DispatchQueue.main.async {
            self.messages.append(message)
            self.messagesCollectionView.reloadData()
            self.messagesCollectionView.scrollToBottom(animated: true)
        }
    }
}

