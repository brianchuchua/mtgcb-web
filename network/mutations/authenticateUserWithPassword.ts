const authenticateUserWithPassword = `mutation authenticateUserWithPassword($username: String!, $password: String!) {
  authenticateUserWithPassword(username: $username, password: $password) {
    token
    item {
      id
      username
    }
  }
}
`;

export default authenticateUserWithPassword;
