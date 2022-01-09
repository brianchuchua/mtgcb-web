import { ResponsiveContainer } from '../components/layout/ResponsiveContainer';
import { Browse } from '../features/browse/Browse';

const BrowsePage: React.FC = () => (
  <ResponsiveContainer maxWidth="xl">
    <Browse />
  </ResponsiveContainer>
);

export default BrowsePage;
