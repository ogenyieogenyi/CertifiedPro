# CertifiedPro
 
# CertifiedPro

A decentralized professional certification verification platform built on Stacks blockchain using Clarity smart contracts.

## Overview

CertifiedPro is a blockchain-based solution that enables professionals to verify, manage, and share their certifications in a trustless manner. The platform includes a skills marketplace, endorsement system, and time-locked credentials to ensure certification validity.

## Features

- **Certification Verification**: Issue and verify professional certifications with immutable records
- **Skills Marketplace**: Connect professionals with specific skills to potential opportunities
- **Endorsement System**: Allow peer validation through a transparent endorsement mechanism
- **Time-locked Credentials**: Automatically enforce certification validity periods

## Smart Contract Architecture

The core functionality is implemented in the `CertifiedPro.clar` Clarity smart contract, which provides the following capabilities:

### Data Structures

- **Certifications**: Stores professional certification details including title, issuer, validity dates, and associated skills
- **User Certifications**: Maps users to their certification IDs for easy lookup
- **Endorsements**: Tracks peer endorsements for certifications with ratings and comments
- **Skills Marketplace**: Organizes professionals by skills for discovery

### Key Functions

#### Public Functions

- `issue-certification`: Create a new professional certification with relevant details
- `endorse-certification`: Add an endorsement to an existing certification
- `revoke-certification`: Deactivate a certification when it's no longer valid

#### Read-Only Functions

- `get-certification`: Retrieve details for a specific certification
- `get-user-certifications`: List all certifications owned by a user
- `get-endorsement`: View endorsement details for a certification
- `is-certification-valid`: Check if a certification is currently valid
- `get-professionals-by-skill`: Find professionals with a specific skill

## Development

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) - Clarity development environment
- [Node.js](https://nodejs.org/) - For running tests with Vitest

### Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/ogenyieogenyi/CertifiedPro.git
   cd CertifiedPro
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

### Testing

The project uses Vitest with the Clarinet SDK for testing. Run tests with:

```bash
npm test
```

The test configuration in `vitest.config.js` is set up to work seamlessly with Clarinet and the Simnet environment.

### Advanced Testing Options

- Collect coverage reports:
  ```bash
  npm test -- --coverage
  ```

- Generate cost analysis:
  ```bash
  npm test -- --costs
  ```

- Use a custom Clarinet manifest:
  ```bash
  npm test -- --manifest ./custom-Clarinet.toml
  ```

## Usage Examples

### Issuing a Certification

```clarity
(contract-call? .CertifiedPro issue-certification 
  "Certified Blockchain Developer" 
  "Blockchain Academy" 
  u1654012800 
  u1717171200 
  (list "Clarity" "Smart Contracts" "Blockchain"))
```

### Endorsing a Certification

```clarity
(contract-call? .CertifiedPro endorse-certification 
  u1 
  u5 
  "Excellent understanding of Clarity smart contracts")
```

### Checking Certification Validity

```clarity
(contract-call? .CertifiedPro is-certification-valid u1)
```

## Security Considerations

- Certifications can only be revoked by their owners
- Endorsements require verification that the endorser is not the certification owner
- Rating values are constrained to ensure data integrity
- Time-locked credentials automatically enforce expiration dates

## Future Enhancements

- Integration with decentralized identity solutions
- Reputation scoring based on endorsement quality
- Certification transfer mechanisms for institutional issuers
- Enhanced privacy features for sensitive certifications
- Subscription model for certification renewal

## License

[MIT License](LICENSE)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Contact

Project Link: [https://github.com/ogenyieogenyi/CertifiedPro](https://github.com/ogenyieogenyi/CertifiedPro)
