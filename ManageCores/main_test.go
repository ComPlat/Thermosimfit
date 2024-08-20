package main

import (
	"testing"
)

func TestRequestCores(t *testing.T) {

	handleInfo("add: FAKETOKEN1")
	handleInfo("add: FAKETOKEN2")
	handleInfo("add: FAKETOKEN3")

	if len(shinySessions) != 3 {
		t.Errorf("Length: %d", len(shinySessions))
	}

	session := getSession("FAKETOKEN1")
	if session.id != "FAKETOKEN1" {
		t.Errorf("ID: %s", session.id)
	}
	if session.cores != 0 {
		t.Errorf("Cores: %d", session.cores)
	}

	handleInfo("request: FAKETOKEN1: 5")
	handleInfo("request: FAKETOKEN2: 3")
	handleInfo("request: FAKETOKEN3: 1")
	session = getSession("FAKETOKEN1")
	if session.cores != 5 {
		t.Errorf("Cores Token1: %d", session.cores)
	}
	session = getSession("FAKETOKEN2")
	if session.cores != 3 {
		t.Errorf("Cores Token2: %d", session.cores)
	}
	session = getSession("FAKETOKEN3")
	if session.cores != 1 {
		t.Errorf("Cores Token3: %d", session.cores)
	}
	if usedCores != 9 {
		t.Errorf("Used Cores: %d", usedCores)
	}

	handleInfo("release: FAKETOKEN1")
	handleInfo("release: FAKETOKEN2")
	handleInfo("release: FAKETOKEN3")
	session = getSession("FAKETOKEN1")
	if session.cores != 0 {
		t.Errorf("Cores Token1: %d", session.cores)
	}
	session = getSession("FAKETOKEN2")
	if session.cores != 0 {
		t.Errorf("Cores Token2: %d", session.cores)
	}
	session = getSession("FAKETOKEN3")
	if session.cores != 0 {
		t.Errorf("Cores Token3: %d", session.cores)
	}
	if usedCores != 0 {
		t.Errorf("Used Cores: %d", usedCores)
	}

	handleInfo("remove: FAKETOKEN1")
	handleInfo("remove: FAKETOKEN2")
	handleInfo("remove: FAKETOKEN3")

	if len(shinySessions) != 0 {
		t.Errorf("Test1: %d", len(shinySessions))
	}
}
