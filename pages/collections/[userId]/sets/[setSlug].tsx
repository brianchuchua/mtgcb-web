import Container from '@material-ui/core/Container';
import { useRouter } from 'next/router';
import { Set } from '../../../../features/collections/sets/Set';

const SetPage: React.FC = () => {
  const router = useRouter();
  const { userId, setSlug } = router.query;
  const id = userId as string;
  const slug = setSlug as string;

  return (
    <Container component="main" maxWidth="xl">
      <Set setSlug={slug} userId={id} />
    </Container>
  );
};

export default SetPage;
