function request(method, path, body, responseType) {
    return new Promise(
        function(resolve, reject) {
            let xhr = new XMLHttpRequest();
            xhr.addEventListener(
                "loadend",
                function() {
                    if (xhr.status == 200) {
                        resolve(xhr.response);
                    } else {
                        reject(xhr);
                    }
                });
            xhr.open(method, path);
            if (responseType)
                xhr.responseType = responseType;
            xhr.send(body);

        }
    )
}

async function init () {
    let video = document.querySelector('video');
    let stream = await navigator.mediaDevices.getUserMedia({audio: false, video: true});

    let pc = new RTCPeerConnection({});

    for(let track of stream.getTracks()) {
        pc.addTrack(track, stream);
    }

    let offer = await pc.createOffer();
    let session_data = await request("POST", "/", "", "json");

    window.setInterval(
        function() {
            request("POST", "/keepalive", JSON.stringify({"session_id": session_data.session_id}), "text");
        },
        30000);

    let event_source = new EventSource("/events/" + session_data.session_id + "/" + session_data.handle_id);

    event_source.onmessage =
        function(event) {
            let message = JSON.parse(event.data);
            if (message.jsep) {
                pc.setRemoteDescription(message.jsep);
            }
        };

    await request(
        "POST",
        "/message",
        JSON.stringify(
            {"session_id": session_data.session_id,
             "handle_id": session_data.handle_id,
             "body": {"audio": false, "video": true},
             "jsep": offer
            }
        ),
        "text");

    pc.addEventListener(
        "icecandidate",
        function(event) {
            request(
                "POST", "/trickle",
                JSON.stringify(
                    {"session_id": session_data.session_id,
                     "handle_id": session_data.handle_id,
                     "candidate": event.candidate || {"completed": true}
                    }),
                "text");
        });


    pc.addEventListener(
        "track",
        function(event) {
            let stream = event.streams[0];
            video.srcObject = stream;
            video.play();
        });

    pc.setLocalDescription(offer);
}

function on_load() {
    init().then(
        function(value) {
            console.log("FINISH", value);
        },
        function(reason) {
            console.log("FINISH", reason);
        });
}


window.addEventListener("load", on_load);
