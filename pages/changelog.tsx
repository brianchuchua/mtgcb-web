import Container from '@material-ui/core/Container';
import Typography from '@material-ui/core/Typography';

const ChangelogPage: React.FC = () => (
  <Container maxWidth="xl">
    <Typography variant="h3" component="h1" align="center">
      Changelog
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-01 (v0.31.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a bug where unique cards completed weren't being calculated properly on the Set collection view.</li>
        <li>Made it clearer which pages are still not yet implemented.</li>
      </ul>
    </Typography>
  </Container>
);

export default ChangelogPage;
