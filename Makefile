DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")
ELN_DIR= ".cache/var/"

-include $(DRONES_DIR)/borg/borg.mk

.PHONY: bootstrap-borg update-all build-all build-update clean-all menu

bootstrap-borg:
	@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
		--url https://github.com/emacscollective/borg.git
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/master
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD

move-eln:
	@if [ -d "eln-cache" ]; then cp -r eln-cache $(ELN_DIR) && rm -rf eln-cache; fi

update-all:
	git submodule update --remote --jobs $(shell nproc)

build-all:
	@make $(DRONES_DIR)/*
	@make move-eln

build-update:
	@make $(shell git status -s --porcelain -- $(DRONES_DIR) | perl -ne '/^( M|M |A )\s*(.*)$$/m && printf " %s", $$2')
	@make move-eln

clean-all:
	@make clean
	@make move-eln

menu::
	$(info make bootstrap-borg  = bootstrap borg itself)
	$(info make bootstrap       = bootstrap collective or new drones)
	$(info make build-all       = rebuild all drones)
	$(info make $(DRONES_DIR)/DRONE       = rebuild DRONE)
	$(info make update-all      = update all drones)
	$(info make build-update    = rebuild updated drones)
	$(info make clean-all       = remove all byte-code files)
	@true
