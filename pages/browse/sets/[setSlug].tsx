import { useRouter } from 'next/router';
import { ResponsiveContainer } from '../../../components/layout/ResponsiveContainer';
import { Set } from '../../../features/sets/Set';

const SetPage: React.FC = () => {
  const router = useRouter();
  const { setSlug } = router.query;
  const slug = setSlug as string;

  return (
    <ResponsiveContainer maxWidth="xl">
      <Set setSlug={slug} />
    </ResponsiveContainer>
  );
};

export default SetPage;
