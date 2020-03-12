//
//  Message.swift
//  ScaledroneChat
//
//  Created by Bobby Dhillon on 3/12/20.
//  Copyright © 2020 Scaledrone. All rights reserved.
//

import Foundation
import UIKit
import MessageKit

struct Message {
    let member: Member
    let text: String
    let messageId: String
}

extension Message: MessageType {
    
    var sender: Sender {
        return Sender(id: member.name, displayName: member.name)
    }
    
    var sentDate: Date {
        return Date()
    }
    
    var kind: MessageKind {
        return .text(text)
    }
}

struct Member {
    let name: String
    let color: UIColor
}
