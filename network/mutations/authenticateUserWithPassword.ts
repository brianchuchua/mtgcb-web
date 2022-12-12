const authenticateUserWithPassword = `mutation authenticateUserWithPassword($username: String!, $password: String!) {
  authenticateUserWithPassword(username: $username, password: $password) {
    ...on UserAuthenticationWithPasswordSuccess {
      token:sessionToken
      item {
        id
        username
        email
      }
    }
    ...on UserAuthenticationWithPasswordFailure {
      message
    }
  }
}
`;

export default authenticateUserWithPassword;
