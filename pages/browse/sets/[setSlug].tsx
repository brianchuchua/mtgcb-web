import Container from '@material-ui/core/Container';
import { useRouter } from 'next/router';
import { Set } from '../../../features/sets/Set';

const SetPage: React.FC = () => {
  const router = useRouter();
  const { setSlug } = router.query;
  const slug = setSlug as string;

  return (
    <Container component="main" maxWidth="xl">
      <Set setSlug={slug} />
    </Container>
  );
};

export default SetPage;
