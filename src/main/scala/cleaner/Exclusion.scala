package cleaner

// хранит исключение, например папку fixtures которую не нужно очищать
case class Exclusion(id: String, kind: String, value: String, scope: String)
