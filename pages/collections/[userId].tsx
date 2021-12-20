import Container from '@material-ui/core/Container';
import { useRouter } from 'next/router';
import { Collection } from '../../features/collections/Collection';

const CollectionPage: React.FC = () => {
  const router = useRouter();
  const { userId } = router.query;
  const id = userId as string;

  return (
    <Container component="main" maxWidth="xl">
      {id ? <Collection userId={id} /> : null}
    </Container>
  );
};

export default CollectionPage;
