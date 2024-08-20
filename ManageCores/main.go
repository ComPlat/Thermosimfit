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

// String helper function
func processString(s string) string {
	s = strings.Replace(s, "\n", "", -1)
	s = strings.TrimSpace(s)
	return s
}

// Session list
type Session struct {
	id           string
	cores        int
	priority     int
	desiredCores int
}

// Status
const (
	OK              = "OK"
	TOOMANYCORES    = "Exceeded core limit"
	CORESALLOCATED  = "Cores allocated"
	NOTENOUGHCORES  = "Not enough cores"
	SESSIONNOTFOUND = "Session not found"
)

// Core list
var totalCores = runtime.NumCPU() - 1
var usedCores = 0

// Shiny-Session list
var shinySessions = make(map[string]Session)
var mutex = &sync.Mutex{}

func getSession(s string) Session {
	session := shinySessions[s]
	return session
}

func updateSession(cores int, s string) {
	shinySessions[s] = Session{s, cores, -1, 0}
}

func updateSessionPrio(prio int, desiredCore int, s string) {
	shinySessions[s] = Session{s, 0, prio, desiredCore}
}

// Request cores
func requestCores(session *Session) string {
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
	usedCores -= session.cores
	session.cores = 0
}

// Handle info from shiny
func handleInfo(info string) (string, error) {
	mutex.Lock()
	defer mutex.Unlock()
	status := OK
	split := strings.Split(info, ":")
	if (len(split) != 2) && (len(split) != 3) {
		return status, errors.New("Invalid input from shiny")
	}
	if len(split) == 2 {
		split[0] = processString(split[0])
		split[1] = processString(split[1])
	} else if len(split) == 3 {
		split[0] = processString(split[0])
		split[1] = processString(split[1])
		split[2] = processString(split[2])
	}

	if split[0] == "add" {
		shinySessions[split[1]] = Session{split[1], 0, -1, 0}
	} else if split[0] == "remove" {
		session := shinySessions[split[1]]
		releaseCores(&session)
		delete(shinySessions, split[1])
	} else if split[0] == "request" {
		cores, err := strconv.Atoi(split[2])
		if err != nil {
			return status, err
		}
		session := getSession(split[1])
		session.cores = cores
		status = requestCores(&session)
		if status == CORESALLOCATED {
			updateSession(cores, split[1])
		} else if status == NOTENOUGHCORES {
			updateSessionPrio(session.priority+1, cores, split[1])
		}
	} else if split[0] == "release" {
		session := getSession(split[1])
		releaseCores(&session)
		updateSession(0, split[1])
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
		fmt.Println(usedCores) // TODO: remove this
		conn, err := ln.Accept()
		if err != nil {
			errCh <- err
		}
		status := readRInfo(conn, errCh)
		sendRInfo(conn, status)
		conn.Close()
	}
}

// TODO: use this later to allocate cores to sessions with highest priority
// Session with highest priority available
func getHighestPriority() string {
	prio := -1
	id_max_prio := ""
	for id, session := range shinySessions {
		if session.priority > prio {
			prio = session.priority
			id_max_prio = id
		}
	}
	session := getSession(id_max_prio)
	if session.priority >= 0 && session.desiredCores <= (totalCores-usedCores) {
		return id_max_prio
	}
	return ""
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
