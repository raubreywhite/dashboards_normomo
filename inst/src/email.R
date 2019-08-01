print(file.exists("/etc/gmailr/blastula.txt"))

print(blastula::creds_file("/etc/gmailr/blastula.txt"))

fd::msg("attempting email")

email <- blastula::compose_email(body ="cannot start processx process")

blastula::smtp_send(
  email,
  from = "dashboardsfhi@gmail.com",
  to = "riwh@fhi.no",
  subject = "This is a test email.",
  credentials = blastula::creds_file("/etc/gmailr/blastula.txt")
)
