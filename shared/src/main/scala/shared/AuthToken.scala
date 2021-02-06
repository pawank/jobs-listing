package models

import java.time.ZonedDateTime
import java.util.UUID

/**
 * A token to authenticate a user against an endpoint for a short time period.
 *
 * @param id The unique token ID.
 * @param userID The unique ID of the user the token is associated with.
 * @param expiry The date-time the token expires.
 */
case class AuthToken(
                      id: UUID,
                      userID: UUID,
                      expiry: ZonedDateTime,
                      //token: String,
                      //username: String,
                      _key: Option[String] = None
)
