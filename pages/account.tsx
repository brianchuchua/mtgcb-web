import Container from '@material-ui/core/Container';
import { Account } from '../features/account/Account';

const AccountPage: React.FC = () => (
  <Container component="main" maxWidth="lg">
    <Account />
  </Container>
);

export default AccountPage;
