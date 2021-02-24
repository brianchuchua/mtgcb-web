import Container from '@material-ui/core/Container';
import { Browse } from '../features/browse/Browse';

const BrowsePage: React.FC = () => (
  <Container component="main" maxWidth="xl">
    <Browse />
  </Container>
);

export default BrowsePage;
