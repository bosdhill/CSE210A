# P2P Messaging App

This was a programming project for CSE210A at UCSC. I chose to implement a P2P Messaging Application in:
- Swift 4
- Java
- Kotlin

with the goal of facilitating direct text exchange between an iOS and Android device and to analyze/compare their respective implementations.

# Installation, Deployment, and Usage

## Mobile Device Requirements
- Android: SDK verison 26 and above with latest version of OS
- iOS: iPhone 5s or newer with latest version of OS

## Computer Requirements
- For 2 Android devices:
    - Computer with the latest version of Android Studio
- For 2 iOS devices:
    - Mac with the latest version of Xcode
- For 1 Android and 1 iOS*:
    - Mac with the latest version of Xcode and Android Studio

*preferred, as this was my configuration

## Installation

### iOS
1. Enable Developer Mode
    - Enable `Developer mode` on your iOS device (instructions [here](https://apple.stackexchange.com/questions/159196/enable-developer-inside-the-settings-app-on-ios))
2. Deploy
    - Plug your iOS device into your Mac.
    - Open Xcode, then navigate to `File > Open` and open `path/to/p2pchat/swift`. You should see the iOS device recognized in the upper left hand corner of the Xcode window upon opening the project.
    - Xcode should automatically download and install any `pods`. If not, navigate to `path/to/p2pchat/swift` and install `pods` with `pod install`.
    - Select the `p2pchat` project (should be the root directory with a blue icon) and go to `Identity > Bundle Identifier` and update the  `Bundle Identifier` to something unique (you can concatenate the identifer with 1's). You should see `Signing` beneath `Identity`; select `Automatically manage signing` and select your `Apple ID` as the `Team`.
    - Press `Command + R` to deploy and run the application on your iOS device. You will need [trust](https://testersupport.usertesting.com/hc/en-us/articles/115003712912-How-to-Trust-an-Unreleased-iOS-App) the application from your iOS device the first time you run it.
    - You should see a `Hype is started...`  dialog when the application opens.

### Android
For Android, you can choose to install and deploy either `path/to/p2pchat/kotlin` or `path/to/p2pchat/java` which have identical behavior.

1. Enable Developer Mode
    - Enable `Developer mode` with `USB Debugging` on your Android device (instructions [here](https://www.howtogeek.com/129728/how-to-access-the-developer-options-menu-and-enable-usb-debugging-on-android-4.2/))
2. Deploy
    - Plug your Android device into your computer.
    - Open Android Studio, then navigate to `Open an existing Android Studio project` and open `path/to/p2pchat/kotlin` or `path/to/p2pchat/java`. You should see the Android device recognized near the middle of the top bar.
    - Android Studio should automatically run `gradle build` and install any dependencies.
    - Press `Ctrl + R` to deploy and run the application on your Android device.
    - You should see a `Hype is started...`  dialog when the application opens.


## Usage

1. Ensure both devices are connected to the same wireless network and have bluetooth turned on.
2. Start the application on one of the devices and wait for the `Hype is started...` dialog to display.
3. Start the application on the other device and wait for the `Hype is started...` dialog to display.
4. After a short while, a `Hype new instance resolved` dialog should display on both applications. Select `Ok` to intiate communication.
5. You may now send and receive messages between devices.

## Common Errors
1. Hype resolves and loses instances in a loop:
    - In this case, restart the apps on both phones.
    - If that doesn't work, check if they are both connected to the wifi and have bluetooth on.
    - If that doesn't work, try reinstalling the applications.
2. `Hype is started...` displays for a long period of time:
    - In this case, try everything mentioned in 1.
3. Uncommon, but when Hype never starts:
    - In this case, try everything mentioned in 1.
    - If that doesn't work, I'm not sure what to suggest, other than ensuring wifi connectivity. I had to email support back and forth and could never quite figure out why that happened. It seems to be a common issue.
    - If that doesn't work and you are really keen to get it working, you could create a hypelabs.io account and use your authentication token and application identifier and replace them with mine in the app. This might work, since I am allowed only up to 10 provisioned devices under my development account.



