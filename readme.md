# Erlang Distributed Client-Server System

## Overview

This project builds a simple distributed system in Erlang with:

This project implements a distributed client-server system in Erlang.
The `server` module acts as a central node for storing and retrieving key-value pairs,
while the `client` module represents distributed nodes that communicate with neighbors and relay data to the server.
The system demonstrates concepts such as process monitoring, message passing, and distributed communication.

---

## Features
- **Central Server**: Stores and retrieves key-value pairs.
- **Clients**: Relay messages to neighbors and periodically send data to the server.
- **Distributed Communication**: Nodes communicate across different machines using cookies.
- **Fault Tolerance**: Monitors processes and handles failures gracefully.

---

## Requirements

- Erlang/OTP installed.
- All nodes must use the same cookie (e.g., `sismdcookie`).
- Nodes must be able to communicate (use `-sname` and ensure hostname resolution).

---

## How to Run

### 1. Start the Server

Open terminal:

```bash
erl -sname central -setcookie sismdcookie
```

In the Erlang shell:

```erlang
c(server).
server:start().
```

---

### 2. Start a Relay Client (No data generation)

In a new terminal:

```bash
erl -sname client1 -setcookie sismdcookie
```

In the shell:

```erlang
c(client).
client:start(client1, [{client1, 'client2@<YourHost>'}, {central, 'central@<YourHost>'}]).
```

---

### 3. Start a Sensor Client (Sends data every 3 seconds)

Open another terminal:

```bash
erl -sname client2 -setcookie sismdcookie
```

In the shell:

```erlang
c(client).
client:start(client2, [{client1, 'client1@<YourHost>'}, {central, 'central@<YourHost>'}], 3000).
```

> Replace `<YourHost>` with your actual machine hostname.

---

## Lookup Stored Data

In the server shell:

```erlang
server:lookup("temperature").
```

---

## Stopping

In the client shells:

```erlang
client:stop(client1).
client:stop(client2).
```

In the server shell:

```erlang
server:stop().
```

---

## Notes
- You can add more clients by repeating the steps above.
