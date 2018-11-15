default: main
.PHONY: default

ifndef PUREC_DIR
$(error '$$PUREC_DIR not set')
endif

include $(PUREC_DIR)/mk/target.mk

$(eval $(call purs_mk_target,main,Test.Main,src test,))
