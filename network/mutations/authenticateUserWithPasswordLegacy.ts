const authenticateUserWithPasswordLegacy = `mutation authenticateUserWithPasswordLegacy($username: String!, $password: String!){
  authenticateUserWithPasswordLegacy(username: $username, password: $password){
    readyForLogin
    reason
  }
}`;

export default authenticateUserWithPasswordLegacy;
