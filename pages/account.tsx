import Container from '@material-ui/core/Container';
import { Account } from '../features/account/Account';

// TODO: Check the maxwidth here
const AccountPage: React.FC = () => (
  <Container component="main" maxWidth="lg">
    <Account />
  </Container>
);

export default AccountPage;
