package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"net"
	"os"
	"runtime"
	"strconv"
	"strings"
	"sync"
)

// Session list
type Session struct {
	id    string
	cores int
}

// Status
const (
	OK             = "OK"
	TOOMANYCORES   = "Exceeded core limit"
	CORESALLOCATED = "Cores allocated"
	NOTENOUGHCORES = "Not enough cores"
)

// Core list
var totalCores = runtime.NumCPU()
var usedCores = 0

// Shiny-Session list
var shinySessions = make(map[string]Session)
var mutex = &sync.Mutex{}

// Request cores
func requestCores(session *Session) string {
	mutex.Lock()
	defer mutex.Unlock()
	if session.cores > 0 && session.cores <= (totalCores-usedCores) {
		usedCores += session.cores
		return CORESALLOCATED
	} else if session.cores > totalCores {
		return TOOMANYCORES
	} else {
		return NOTENOUGHCORES
	}
}

// Release cores
func releaseCores(session *Session) {
	mutex.Lock()
	defer mutex.Unlock()
	usedCores -= session.cores
	session.cores = 0
}

// Handle info from shiny
func handleInfo(info string) (string, error) {
	status := OK
	split := strings.Split(info, ":")
	if (len(split) != 2) && (len(split) != 3) {
		return status, errors.New("Invalid input from shiny")
	}
	if split[0] == "add" {
		mutex.Lock()
		shinySessions[split[1]] = Session{split[1], 0}
		mutex.Unlock()
	} else if split[0] == "remove" {
		mutex.Lock()
		session := shinySessions[split[1]]
		releaseCores(&session)
		delete(shinySessions, split[1])
		mutex.Unlock()
	} else if split[0] == "request" {
		s := strings.Replace(split[2], "\n", "", -1)
		cores, err := strconv.Atoi(s)
		if err != nil {
			return status, err
		}
		mutex.Lock()
		session := shinySessions[split[1]]
		session.cores = cores
		shinySessions[split[1]] = session
		mutex.Unlock()
		status = requestCores(&session)
	} else if split[0] == "release" {
		mutex.Lock()
		session := shinySessions[split[1]]
		releaseCores(&session)
		shinySessions[split[1]] = session
		mutex.Unlock()
	}
	return status, nil
}

// Read info from shiny app
func readRInfo(conn net.Conn, errCh chan<- error) string {
	reader := bufio.NewReader(conn)
	message, _ := reader.ReadString('\n')
	status, err := handleInfo(message)
	if err != nil {
		errCh <- err
	}
	return status
}

// Send info to shiny app
func sendRInfo(conn net.Conn, info string) error {
	_, err := fmt.Fprintln(conn, info)
	return err
}

func runTCP(errCh chan<- error) {
	ln, err := net.Listen("tcp", ":8080")
	if err != nil {
		errCh <- err
	}
	defer ln.Close()

	for {
		conn, err := ln.Accept()
		if err != nil {
			errCh <- err
		}
		status := readRInfo(conn, errCh)
		sendRInfo(conn, status)
		conn.Close()
	}
}

func main() {
	errCh := make(chan error)
	logFile, err := os.OpenFile("log.txt", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatal("Failed to open log file:", err)
	}
	log.SetOutput(logFile)

	go func() {
		for err := range errCh {
			if err != nil {
				log.Println("Error: ", err)
			}
		}
	}()
	runTCP(errCh)
}
