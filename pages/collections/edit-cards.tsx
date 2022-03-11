import { ResponsiveContainer } from '../../components/layout/ResponsiveContainer';
import EditCards from '../../features/edit-cards/EditCards';

const EditCardsPage: React.FC = () => (
  <ResponsiveContainer maxWidth="xl">
    <EditCards />
  </ResponsiveContainer>
);

export default EditCardsPage;
