SET(docbook_specifications_EN
	geomtp_protocol.xml
)

DOCBOOK_TO_HTML(specification docbook_specifications_EN html/specifications/en)

DOCBOOK_TO_PDF(specification docbook_specifications_EN pdf/specifications/en)
