import { ResponsiveContainer } from '../components/layout/ResponsiveContainer';
import { Account } from '../features/account/Account';

const AccountPage: React.FC = () => (
  <ResponsiveContainer maxWidth="xl">
    <Account />
  </ResponsiveContainer>
);

export default AccountPage;
